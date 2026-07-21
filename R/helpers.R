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

# SAGE estimator axis -------------------------------------------------------

# Build the algorithm design for a SAGE implementation.
#
# The three estimators take mutually exclusive budget arguments (passing
# n_permutations with estimator = "kernel" is an error, not a no-op), so the
# estimator axis is an rbind of per-estimator sub-designs with NA in the
# inapplicable columns -- never a CJ over all of them.
#
# conf             the lane's conf list
# sampler          character vector to cross-join, or NULL for marginal methods
# estimators       which estimators this implementation supports. fippy has only
#                  the permutation estimator; Python sage has kernel+permutation.
# kernel_variants  which design-matrix variants it supports. Pass NA_character_
#                  for implementations with no variant choice (Python sage's
#                  kernel estimator is always the unbiased one).
sage_algo_design <- function(
	conf,
	sampler = NULL,
	estimators = conf$sage_estimators,
	kernel_variants = conf$kernel_variants
) {
	parts <- list()

	if ("permutation" %in% estimators) {
		parts$permutation <- data.table::CJ(
			estimator = "permutation",
			n_permutations = conf$n_permutations,
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
			n_coalitions = conf$n_coalitions,
			kernel_variant = kernel_variants,
			sage_n_samples = conf$sage_n_samples
		)
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

	if (!is.null(sampler)) {
		# Cross join. data.table's merge has no by = NULL, base merge does.
		d <- data.table::as.data.table(
			merge(as.data.frame(d), data.frame(sampler = sampler, stringsAsFactors = FALSE))
		)
	}

	d[]
}
