# CPUs this job may use. parallelly::availableCores() honors the Slurm allocation
# (SLURM_CPUS_PER_TASK, ncpus=2 by default), cgroup quotas, PBS/SGE, etc., falling
# back to all cores off-cluster. Applied uniformly to every learner/model thread
# count so runtime comparisons stay fair (parity).
n_threads <- function() {
	as.integer(parallelly::availableCores())
}

.ensure_torch <- function() {
	if (!requireNamespace("torch", quietly = TRUE)) {
		cli::cli_abort(c(
			"x" = "The {.pkg torch} package is not installed.",
			"i" = "Run {.code make setup} (or {.code rv sync})."
		))
	}
	# Fail fast with an actionable message: without libtorch the mlp learner dies
	# deep in torch (a cryptic {.code .torch_can_load} error) instead of here.
	if (!torch::torch_is_installed()) {
		cli::cli_abort(c(
			"x" = "libtorch is not installed, so the torch (mlp) learner cannot run.",
			"i" = "Run {.code make torch} (downloads libtorch via {.fn torch::install_torch})."
		))
	}
	invisible(TRUE)
}

# Helper function to create resampling strategy
create_resampling <- function(
	type = "holdout",
	ratio = 2 / 3,
	folds = 3,
	repeats = 10
) {
	switch(
		type,
		"cv" = mlr3::rsmp("cv", folds = folds),
		"holdout" = mlr3::rsmp("holdout", ratio = ratio),
		"subsampling" = mlr3::rsmp("subsampling", ratio = ratio, repeats = repeats),
		"bootstrap" = mlr3::msr("bootstrap", ratio = 1, repeats = repeats)
	)
}

# Helper function to instantiate resampling reproducibly
# Ensures that:
# - Same task + same replication = same splits (for fair method comparison)
# - Same task + different replication = different splits (for independent runs)
instantiate_resampling <- function(resampling, task, replication = 1) {
	# Generate task-specific seed from hash using digest
	task_seed <- digest::digest2int(task$hash)

	# Combine with replication number for variation across replications
	combined_seed <- task_seed + replication

	withr::with_seed(combined_seed, {
		resampling$instantiate(task)
	})

	resampling
}

# Helper function to create learner
create_learner <- function(
	learner_type = c("rf", "linear", "featureless", "mlp", "boosting"),
	n_trees = 500,
	n_units = 20,
	task_type = c("regr", "classif"),
	task = NULL # Optional task to check for categorical features
) {
	requireNamespace("mlr3learners", quietly = TRUE)
	require("mlr3pipelines")
	learner_type <- match.arg(learner_type)
	task_type <- match.arg(task_type)
	needs_encoding <- any(task$feature_types$type %in% c("factor", "character"))

	base_learner <- switch(
		learner_type,
		"featureless" = {
			lrn(paste(task_type, "featureless", sep = "."))
		},
		"rf" = {
			lrn(paste(task_type, "ranger", sep = "."), num.trees = n_trees, num.threads = n_threads())
		},
		"linear" = {
			switch(task_type, regr = lrn("regr.lm"), classif = lrn("classif.log_reg"))
		},
		"mlp" = {
			.ensure_torch()
			require(mlr3torch)
			# Cap torch to the allotted CPUs -- uncapped it grabs the whole node,
			# breaking parity with the single/2-threaded ranger & xgboost learners.
			torch::torch_set_num_threads(n_threads())
			base_learner <- lrn(
				paste(task_type, "mlp", sep = "."),
				# architecture parameters
				neurons = n_units,
				n_layers = 1,
				# training arguments
				batch_size = 18000, # as large as plausible to fit all datasets
				epochs = 500,
				opt.lr = 0.1, # initialize with larger learning rate
				patience = 50,
				measures_valid = switch(task_type, regr = msr("regr.rsq"), classif = msr("classif.acc")),
				min_delta = 0.01,
				shuffle = TRUE,
				tensor_dataset = TRUE, # for optimization when dataset fits in RAM
				device = "cpu"
			)
			# Add encoding, sadly makes predict_newdata_fast impossible
			if (needs_encoding) {
				base_learner <- po("encode", method = "one-hot") %>>%
					base_learner |>
					as_learner()
			}

			set_validate(base_learner, "test")

			base_learner

			# lrn(
			# 	paste(task_type, "nnet", sep = "."),
			# 	size = n_units,
			# 	trace = FALSE
			# )
		},
		"boosting" = {
			base_learner <- lrn(
				paste(task_type, "xgboost", sep = "."),
				nrounds = 1000,
				early_stopping_rounds = 50,
				eta = 0.1,
				booster = "gbtree",
				tree_method = "hist",
				nthread = n_threads()
			)

			# Add encoding, sadly makes predict_newdata_fast impossible
			if (needs_encoding) {
				base_learner <- po("encode", method = "one-hot") %>>%
					base_learner |>
					as_learner()
			}

			set_validate(base_learner, validate = "test")
			base_learner
		}
	)
	base_learner
}

# Helper function to create measure
create_measure <- function(task_type = "regr") {
	importance <- switch(
		task_type,
		"regr" = mlr3::msr("regr.mse"),
		"classif" = mlr3::msr("classif.ce")
	)
	eval <- switch(
		task_type,
		"regr" = mlr3::msr("regr.rsq"),
		"classif" = mlr3::msr("classif.acc")
	)
	list(importance = importance, eval = eval)
}

# Helper function to create conditional sampler
create_sampler <- function(
	sampler = c("arf", "gaussian", "knn", "ctree"),
	task
) {
	sampler <- match.arg(sampler)

	switch(
		sampler,
		"arf" = ConditionalARFSampler$new(
			task,
			verbose = FALSE,
			finite_bounds = "local",
			min_node_size = 20,
			stepsize = 10000
		),
		"gaussian" = ConditionalGaussianSampler$new(task),
		"knn" = ConditionalKNNSampler$new(task, k = 5),
		"ctree" = ConditionalCtreeSampler$new(task)
	)
}

# Helper function to create complete problem instance
# Wraps common logic for all problems: creating learner, measure, resampling
create_problem_instance <- function(
	task,
	job = NULL,
	learner_type,
	resampling_type = "holdout",
	problem_name,
	has_categoricals = FALSE,
	...
) {
	task_type <- task$task_type

	# Create measure
	measures <- create_measure(task_type = task_type)
	# Create and instantiate resampling
	resampling <- create_resampling(type = resampling_type)
	instantiate_resampling(resampling, task, job$repl %||% 1)

	# Return instance with metadata - no learner created here!
	# Algorithms will create their own learners as needed
	list(
		task = task,
		measure = measures$importance,
		measure_eval = measures$eval,
		resampling = resampling,
		# Metadata
		n_features = length(task$feature_names),
		n_samples = task$nrow,
		task_type = task_type,
		learner_type = learner_type,
		resampling_type = resampling_type,
		has_categoricals = has_categoricals,
		... # Additional problem-specific metadata
	)
}

# Kernel SAGE variance blocks -----------------------------------------------

# Draws per variance block of xplainfi's kernel_variant = "original".
#
# Its standard errors are batch means over consecutive blocks of draws (Covert &
# Lee, Section 4.3), so they are NA until TWO blocks have completed -- and
# $importance(ci_method = "montecarlo") aborts on all-NA SEs instead of
# returning them. A coalition budget below 2 * this is therefore not "less
# precise", it is an errored job.
#
# The block size is xplainfi-internal (`check_interval` in SAGE.R's
# .compute_sage_scores_kernel), so it is mirrored here rather than read off the
# object. tests/check-kernel-sage-api.R pins it against the installed package
# from both sides, so upstream drift fails loudly instead of silently
# mis-scaling this grid.
kernel_block_size <- function(n_features) {
	pmax(16L, 4L * as.integer(n_features))
}

# Kernel coalition budgets for one problem, in whole variance blocks.
#
# The grid is block-relative because the estimability floor is too: block size
# grows with n_features, so a single absolute grid is either below the floor on
# the wide problems or far above exact-enumeration cost on the narrow ones. In
# blocks, "2" means the same thing everywhere -- the minimum at which SEs exist.
kernel_budgets <- function(n_features, blocks) {
	checkmate::assert_int(n_features, lower = 1L)
	blocks <- as.integer(blocks)
	if (any(blocks < 2L)) {
		cli::cli_abort(c(
			"{.arg blocks} {.val {blocks[blocks < 2L]}} below the two blocks the batch-means SEs need.",
			i = "Such a row does not produce a wider interval, it aborts in {.code importance(ci_method = \"montecarlo\")}."
		))
	}
	sort(unique(blocks * kernel_block_size(n_features)))
}

# Number of features each registered problem generates.
#
# Needed at setup time to scale the coalition grid per problem, and not
# derivable from the design: it is a property of the DGP. Instantiating each
# problem once is cheap (create_problem_instance() fits no learner) and beats a
# hardcoded table, which a changed DGP would silently invalidate.
#
# Only for lanes whose problems have a FIXED width. The runtime lane sweeps
# n_features on the problem design instead, and keys its grid on that column
# directly -- see kernel_budget_grid(by =).
problem_feature_counts <- function(prob_funs, prob_designs) {
	missing <- setdiff(names(prob_designs), names(prob_funs))
	if (length(missing) > 0) {
		cli::cli_abort("No problem function for design{?s}: {.val {missing}}.")
	}
	data.table::rbindlist(lapply(names(prob_designs), function(nm) {
		# First design row only: n_features is fixed per problem here, and the
		# design axes (n_samples, correlation, learner_type) do not move it.
		args <- as.list(prob_designs[[nm]][1L])
		inst <- do.call(prob_funs[[nm]], c(list(data = NULL, job = NULL), args))
		data.table::data.table(problem = nm, n_features = as.integer(inst$n_features))
	}))
}

# Long table of the kernel coalition budgets each job group is allowed.
#
# `feature_counts` carries `by` plus n_features; `by` is whatever identifies the
# feature count in the job table -- "problem" where each DGP has a fixed width,
# "n_features" in the runtime lane, which sweeps it on the problem design.
kernel_budget_grid <- function(feature_counts, blocks, by = "problem") {
	feature_counts <- data.table::as.data.table(feature_counts)
	checkmate::assert_names(names(feature_counts), must.include = c(by, "n_features"))

	# unique(): in the runtime lane `by` IS "n_features", and grouping by a
	# duplicated name yields an "n_features.1" column instead of one key. The
	# c() wrapper is data.table's requirement for a computed `by`.
	keys <- unique(c(by, "n_features"))
	feature_counts[,
		.(n_coalitions = kernel_budgets(n_features, blocks)),
		by = c(keys)
	]
}

# Drop kernel-SAGE jobs whose coalition budget does not belong to their group.
#
# batchtools crosses one algorithm design with every problem, so the design has
# to carry the UNION of the per-group budgets and the cross terms are removed
# here -- the same pattern as the infeasible-exact-arm removal. Early-stopped
# rows are exempt: their n_coalitions is a ceiling rather than a spend, and is
# deliberately one absolute value across groups.
prune_kernel_budgets <- function(reg, grid, by = "problem") {
	tab <- batchtools::unwrap(batchtools::getJobTable(reg = reg))
	if (!"estimator" %in% names(tab)) {
		return(0L)
	}

	keep <- unique(data.table::as.data.table(grid)[, c(by, "n_coalitions"), with = FALSE])
	fixed <- tab[estimator == "kernel" & !early_stopping]
	drop <- fixed[!keep, on = c(by, "n_coalitions")]
	if (nrow(drop) > 0) {
		batchtools::removeExperiments(drop, reg = reg)
	}

	# An over-eager prune would leave an empty or ragged grid, which reads as
	# "budget swept" in every downstream summary. Check what survived instead.
	left <- batchtools::unwrap(batchtools::getJobTable(reg = reg))[
		estimator == "kernel" & !early_stopping
	]
	expected <- keep[, .(want = .N), by = c(by)]
	got <- left[, .(got = data.table::uniqueN(n_coalitions)), by = c(by)]
	ragged <- merge(expected, got, by = by, all = TRUE)[is.na(got) | got != want]
	if (nrow(ragged) > 0) {
		cli::cli_abort(c(
			"Kernel budget grid is ragged after pruning for {.val {by}} {.val {ragged[[by]]}}.",
			i = "Kept {.val {ragged$got}} of {.val {ragged$want}} budget{?s}; every group needs one row per entry of {.field n_coalition_blocks}."
		))
	}

	nrow(drop)
}

# SAGE estimator axis -------------------------------------------------------

# Build the algorithm design for a SAGE implementation.
#
# The three estimators take mutually exclusive budget arguments (passing
# n_permutations with estimator = "kernel" is an error, not a no-op), so the
# estimator axis is an rbind of per-estimator sub-designs with NA in the
# inapplicable columns -- never a CJ over all of them.
#
# conf                the lane's conf list
# sampler             character vector to cross-join, or NULL for marginal methods
# estimators          which estimators this implementation supports. fippy has only
#                     the permutation estimator; Python sage has kernel+permutation.
# kernel_variants     which design-matrix variants it supports. Pass NA_character_
#                     for implementations with no variant choice (Python sage's
#                     kernel estimator is always the unbiased one).
# kernel_es_variants  which kernel variants additionally get an early-stopped row,
#                     on top of the fixed-budget rows. Defaults to
#                     conf$kernel_es_variants; character(0) means none.
#
# The two kernel variants play different roles here, which is why their coverage
# differs rather than being a ragged accident:
#
#   "original"  is the shipped default and the estimator under test. It gets the
#               fixed-budget curve AND an early-stopped row, so early stopping can
#               be judged against that curve and against the exact arm.
#   "unbiased"  exists only as the numerical bridge to the Python `sage` package,
#               which computes exactly that estimator. Comparing the two packages
#               requires a matched fixed budget on both sides, so an early-stopped
#               row would defeat the purpose of the row. It also cannot converge at
#               any tolerable budget in xplainfi's whole-test-set regime -- upstream
#               measures >30x the model evaluations of `sage` -- so early stopping
#               there just burns the ceiling and warns.
#
# Note the inverse failure mode of the missing-formals bug this guards against
# (see tests/test-sage-algo-design.R): omitting "permutation" from `estimators`
# drops the early_stopping / min_permutations columns from the design entirely,
# so the consuming algo_* function's own defaults silently take over instead.
sage_algo_design <- function(
	conf,
	sampler = NULL,
	estimators = conf$sage_estimators,
	kernel_variants = conf$kernel_variants,
	kernel_es_variants = conf$kernel_es_variants
) {
	# conf carries unsuffixed numeric literals, so coerce once here: otherwise a
	# column's type depends on which estimators were requested (rbindlist upcasts
	# NA_integer_ to double only when a real double chunk is present).
	n_permutations <- as.integer(conf$n_permutations)
	n_coalitions <- as.integer(conf$n_coalitions)

	# A typo here would otherwise return an empty design and silently register
	# zero jobs, which reads as "covered" in every downstream summary.
	valid <- c("permutation", "kernel", "exact")
	if (!all(estimators %in% valid)) {
		cli::cli_abort(
			"Unknown {.arg estimators}: {.val {setdiff(estimators, valid)}}. Must be one of {.val {valid}}."
		)
	}

	# A permutation budget at or below min_permutations can never reach the
	# convergence check (xplainfi guards it with `n_completed >= max(min_permutations,
	# 2L)`), so the row spends its whole budget and reports converged = FALSE --
	# numerically identical to a fixed-budget row, but labelled as an early-stopping
	# arm. xplainfi validates the two arguments independently and never against each
	# other, so nothing downstream catches this. Only bites once early stopping is on.
	if (isTRUE(conf$sage_early_stopping) && "permutation" %in% estimators) {
		min_perms <- as.integer(conf$min_permutations)
		incoherent <- n_permutations[n_permutations <= min_perms]
		if (length(incoherent) > 0) {
			cli::cli_abort(c(
				"{.field n_permutations} {.val {incoherent}} at or below {.field min_permutations} = {.val {min_perms}}.",
				i = "With {.code sage_early_stopping = TRUE} such a budget never reaches the convergence check: it spends in full and reports {.code converged = FALSE}, indistinguishable from a fixed-budget row.",
				i = "Raise {.field n_permutations} above {.field min_permutations}, or lower {.field min_permutations}."
			))
		}
	}

	parts <- list()

	if ("permutation" %in% estimators) {
		parts$permutation <- data.table::CJ(
			estimator = "permutation",
			n_permutations = n_permutations,
			n_coalitions = NA_integer_,
			kernel_variant = NA_character_,
			sage_n_samples = conf$sage_n_samples,
			early_stopping = conf$sage_early_stopping,
			min_permutations = conf$min_permutations
		)
	}

	if ("kernel" %in% estimators) {
		parts$kernel <- data.table::CJ(
			estimator = "kernel",
			n_permutations = NA_integer_,
			n_coalitions = n_coalitions,
			kernel_variant = kernel_variants,
			sage_n_samples = conf$sage_n_samples,
			early_stopping = FALSE
		)

		# Early-stopped rows: the budget becomes a ceiling rather than a spend, so
		# it is set well above the fixed grid and `converged` records whether the
		# criterion was met before hitting it.
		es_variants <- intersect(kernel_es_variants %||% character(), kernel_variants)
		if (length(es_variants) > 0) {
			# The ceiling is one absolute value while the fixed grid is now
			# block-relative and resolved per problem, so their ordering is no longer
			# obvious by inspection. A ceiling inside the fixed grid would make the ES
			# arm indistinguishable from a fixed-budget row it is meant to be judged
			# against, and a ceiling below it is simply a smaller budget mislabelled.
			ceiling_draws <- as.integer(conf$n_coalitions_ceiling)
			if (ceiling_draws <= max(n_coalitions)) {
				cli::cli_abort(c(
					"{.field n_coalitions_ceiling} {.val {ceiling_draws}} is not above the fixed kernel grid (max {.val {max(n_coalitions)}}).",
					i = "The early-stopping arm's budget must be a ceiling it can stop below, not a point on the curve it is compared against."
				))
			}
			# The ES rows are the ONLY place se_threshold is swept rather than matched,
			# because they are the only rows that run convergence detection at all --
			# and in this lane the only ES rows anywhere (sage/fippy get
			# kernel_es_variants = character(), and sage_early_stopping = FALSE drops
			# the permutation ES rows), so nothing cross-implementation is matched
			# against them. Measured at the shared 0.025: kernel-original stops at
			# exactly two variance blocks in every configuration tried, because that is
			# the first checkpoint at which its batch-means SEs exist at all. One
			# threshold therefore yields one constant, 2 * kernel_block_size(), and says
			# nothing about where the estimator converges. Sweeping it turns the arm
			# into a budget-vs-tolerance curve, which is the question it exists to
			# answer. Defaults to the matched single value for lanes that do not opt in.
			parts$kernel_es <- data.table::CJ(
				estimator = "kernel",
				n_permutations = NA_integer_,
				n_coalitions = as.integer(conf$n_coalitions_ceiling),
				kernel_variant = es_variants,
				sage_n_samples = conf$sage_n_samples,
				early_stopping = TRUE,
				se_threshold = as.numeric(conf$es_se_thresholds %||% conf$se_threshold)
			)
		}
	}

	if ("exact" %in% estimators) {
		parts$exact <- data.table::CJ(
			estimator = "exact",
			n_permutations = NA_integer_,
			n_coalitions = NA_integer_,
			kernel_variant = NA_character_,
			sage_n_samples = conf$sage_n_samples
		)
	}

	d <- data.table::rbindlist(parts, fill = TRUE)

	# Convergence threshold for early stopping, matched across every implementation
	# (xplainfi's `se_threshold`, sage's and fippy's `thresh`) so an ES comparison
	# is fair -- all three use the same spread-relative max(se)/spread < threshold
	# criterion. 0.025 is now the default of both xplainfi and sage (fippy alone
	# still defaults to a stricter 0.01, which needed ~6x more draws and read as
	# "non-convergence"). Inert on fixed-budget and exact rows, which never run
	# convergence detection.
	#
	# Fill only where the ES block above did not already set a swept value -- a
	# blanket assignment would overwrite the sweep with the matched constant.
	if (!("se_threshold" %in% names(d))) {
		d[, se_threshold := NA_real_]
	}
	d[is.na(se_threshold), se_threshold := as.numeric(conf$se_threshold)]

	if (!is.null(sampler)) {
		# Cross join. data.table's merge has no by = NULL, base merge does.
		d <- data.table::as.data.table(
			merge(as.data.frame(d), data.frame(sampler = sampler, stringsAsFactors = FALSE))
		)
	}

	d[]
}
