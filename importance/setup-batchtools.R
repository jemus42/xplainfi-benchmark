# Main experiment setup and execution script
# Problem definitions for batchtools experiment
library(batchtools)
library(mlr3)
library(data.table)

# Load configuration
source(here::here("setup-common.R"))
source(here::here("importance", "config.R"))


# Create or load registry
# if (dir.exists(conf$reg_path)) {
# 	cli::cli_alert_danger("Deleting existing registry at {.file {fs::path_rel(conf$reg_path)}}")
# 	fs::dir_delete(conf$reg_path)
# }

if (!fs::dir_exists(conf$reg_path)) {
	cli::cli_alert_info("Creating registry at {.file {fs::path_rel(conf$reg_path)}}")
	reg <- makeExperimentRegistry(
		file.dir = conf$reg_path,
		packages = c("mlr3learners", "xplainfi"),
		seed = conf$seed,
		source = here::here(c("R/helpers.R", "R/helpers-python.R", "importance/config.R"))
	)
} else {
	cli::cli_alert_warning("Loading existing registry at {.file {fs::path_rel(conf$reg_path)}}")
	reg <- loadRegistry(conf$reg_path, writeable = TRUE)
}

# All R/ helpers (problems, algorithms, provenance, ...) are loaded by
# setup-common.R via source_r() above.

# ============================================================================
# Register Problems with batchtools
# ============================================================================

# Named list rather than bare addProblem() calls so problem_feature_counts()
# below can instantiate each one to size the kernel coalition grid.
prob_funs <- list(
	ewald = prob_ewald,
	correlated = prob_correlated,
	interactions = prob_interactions,
	bike_sharing = prob_bike_sharing,
	friedman1 = prob_friedman1,
	independent = prob_independent,
	confounded = prob_confounded,
	mediated = prob_mediated
)

for (nm in names(prob_funs)) {
	addProblem(name = nm, data = NULL, fun = prob_funs[[nm]], seed = conf$seed)
}

# ============================================================================
# Register Algorithms with batchtools
# ============================================================================

# All available algorithms. Provider (xplainfi vs reference) is derived from the
# name by convention (see R/provenance.R), so adding e.g. a kernel SAGE method is
# a single entry here plus a matching algo_designs entry below.
algo_funs <- list(
	PFI = algo_PFI,
	CFI = algo_CFI,
	LOCO = algo_LOCO,
	MarginalSAGE = algo_MarginalSAGE,
	ConditionalSAGE = algo_ConditionalSAGE,
	PFI_iml = algo_PFI_iml,
	PFI_vip = algo_PFI_vip,
	PFI_fippy = algo_PFI_fippy,
	CFI_fippy = algo_CFI_fippy,
	MarginalSAGE_fippy = algo_MarginalSAGE_fippy,
	ConditionalSAGE_fippy = algo_ConditionalSAGE_fippy,
	MarginalSAGE_sage = algo_MarginalSAGE_sage
)

# Restrict to the requested providers (conf$providers, default "all").
active_algos <- select_algorithms(names(algo_funs), conf$providers)

# Second, independent filter: which methods this run is about at all. Composes
# with the provider filter above -- providers select implementations, methods
# select the importance measures.
unknown_methods <- setdiff(conf$methods, names(algo_funs))
if (length(unknown_methods) > 0) {
	cli::cli_abort(
		"Unknown {.arg conf$methods}: {.val {unknown_methods}}. Must be one of {.val {names(algo_funs)}}."
	)
}
active_algos <- intersect(active_algos, conf$methods)

cli::cli_alert_info(
	"Providers: {.val {conf$providers}} -> {length(active_algos)} algorithm(s): {.val {active_algos}}"
)

for (nm in active_algos) {
	addAlgorithm(name = nm, fun = algo_funs[[nm]])
}

# ============================================================================
# Problem Designs
# ============================================================================

prob_designs <- list(
	# Friedman1: fixed 10 features, varying sample sizes
	friedman1 = CJ(
		n_samples = conf$n_samples,
		learner_type = conf$learner_types
	),

	# Bike sharing: real-world data, fixed dimensions
	bike_sharing = CJ(
		# n_samples = conf$n_samples,
		learner_type = conf$learner_types,
		convert_to_numeric = TRUE # Convert factors to numeric for fair algorithm comparison
	),

	# Correlated features DGP: varying correlation strength
	correlated = CJ(
		n_samples = conf$n_samples,
		correlation = conf$correlation,
		learner_type = conf$learner_types
	),

	# Ewald DGP: fixed structure
	ewald = CJ(
		n_samples = conf$n_samples,
		learner_type = conf$learner_types
	),

	# Interactions DGP: fixed structure
	interactions = CJ(
		n_samples = conf$n_samples,
		learner_type = conf$learner_types
	),

	independent = CJ(
		n_samples = conf$n_samples,
		learner_type = conf$learner_types
	),

	confounded = CJ(
		n_samples = conf$n_samples,
		learner_type = conf$learner_types
	),

	mediated = CJ(
		n_samples = conf$n_samples,
		learner_type = conf$learner_types
	)
)

# ============================================================================
# Kernel coalition grid (per problem)
# ============================================================================

# Block-relative coalition budgets, as in the validation lane: kernel_variant =
# "original" has no standard errors below two blocks of max(16, 4 * n_features)
# draws, and the job then errors in importance(ci_method = "montecarlo") rather
# than reporting a wider interval. See conf$n_coalition_blocks.
kernel_grid <- kernel_budget_grid(
	problem_feature_counts(prob_funs, prob_designs),
	blocks = conf$n_coalition_blocks
)

conf$n_coalitions <- sort(unique(kernel_grid$n_coalitions))

cli::cli_h2("Kernel Coalition Budgets")
print(kernel_grid[,
	.(
		block = kernel_block_size(n_features[1L]),
		budgets = paste(sort(n_coalitions), collapse = ", "),
		exact_evals = 2^n_features[1L]
	),
	by = .(problem, n_features)
])

# ============================================================================
# Algorithm Designs
# ============================================================================

algo_designs <- list(
	# PFI: Permutation Feature Importance
	PFI = data.table(
		n_repeats = conf$n_repeats
	),

	# CFI: Conditional Feature Importance (with samplers)
	CFI = CJ(
		n_repeats = conf$n_repeats,
		sampler = conf$samplers
	),

	# LOCO: Leave-One-Covariate-Out
	# Fixed at 1: LOCO refits per feature, repeats would only duplicate work
	LOCO = data.table(
		n_repeats = 1L
	),

	# SAGE estimator axis. The three estimators take mutually exclusive budget
	# arguments, so these designs are an rbind of per-estimator sub-designs with
	# NA in the inapplicable columns -- see sage_algo_design() in R/helpers.R.
	MarginalSAGE = sage_algo_design(conf),

	ConditionalSAGE = sage_algo_design(conf, sampler = conf$samplers),

	# Python sage: kernel + permutation. Its kernel estimator is always the
	# unbiased variant, so it has no variant choice, and it has no exact arm.
	# Fixed budget only: this arm is the numerical bridge to xplainfi's
	# kernel_variant = "unbiased", and that comparison needs matched budgets on
	# both sides. sage's Explanation also does not report the effort it spent, so
	# with convergence detection the realised cost would be unknown.
	MarginalSAGE_sage = sage_algo_design(
		conf,
		estimators = c("permutation", "kernel"),
		kernel_variants = NA_character_,
		kernel_es_variants = character()
	),

	# fippy implements the permutation estimator only. `estimator` is set
	# explicitly so the column is present on every SAGE row and the analysis
	# join stays uniform across arms.
	MarginalSAGE_fippy = sage_algo_design(
		conf,
		sampler = "simple",
		estimators = "permutation"
	),

	ConditionalSAGE_fippy = sage_algo_design(
		conf,
		sampler = "gaussian",
		estimators = "permutation"
	),

	# PFI_iml: Reference implementation from iml package
	PFI_iml = data.table(
		n_repeats = conf$n_repeats
	),

	# PFI_vip: Reference implementation from vip package
	PFI_vip = data.table(
		n_repeats = conf$n_repeats
	),

	# PFI_fippy: Reference implementation from fippy package (Python)
	# Use simple sampler (most basic, works with any data type)
	PFI_fippy = CJ(
		n_repeats = conf$n_repeats,
		sampler = "simple"
	),

	# CFI_fippy: Conditional FI from fippy package (Python)
	# Use gaussian sampler (all tasks now have numeric features only)
	CFI_fippy = CJ(
		n_repeats = conf$n_repeats,
		sampler = "gaussian"
	)
)

# ============================================================================
# Add Experiments
# ============================================================================

cli::cli_h1("Adding Experiments to Registry")

# Only add designs for the algorithms that were registered for these providers.
algo_designs <- algo_designs[active_algos]

addExperiments(
	prob.designs = prob_designs,
	algo.designs = algo_designs,
	repls = conf$repls
)

# ============================================================================
# Remove incompatible sampler-task combinations
# ============================================================================

# No sampler incompatibilities to handle anymore since all tasks have numeric features
# (bike_sharing now converts factors to numeric with convert_to_numeric = TRUE)

# Featureless learner is only used for xplainfi runtime benchmarking
featureless_non_xplainfi_jobs <- unwrap(getJobTable())[
	learner_type == "featureless" &
		algorithm %in% c("PFI", "CFI", "MarginalSAGE", "ConditionalSAGE", "LOCO"),
]

if (nrow(featureless_non_xplainfi_jobs) > 0) {
	cli::cli_alert_warning(
		"Removing {nrow(featureless_non_xplainfi_jobs)} job(s) for other methods with featureless learner"
	)
	removeExperiments(featureless_non_xplainfi_jobs)
}

# ============================================================================
# Prune kernel budgets that do not belong to their problem
# ============================================================================

n_pruned <- prune_kernel_budgets(reg, kernel_grid)
if (n_pruned > 0) {
	cli::cli_alert_warning(
		"Removed {n_pruned} kernel job(s) whose coalition budget belongs to another problem's grid"
	)
}

# ============================================================================
# Remove infeasible exact-estimator jobs
# ============================================================================

# estimator = "exact" enumerates 2^n_features coalitions. bike_sharing has 12
# features -- exactly at max_features (12L), so the exact arm is feasible here,
# not infeasible. It is excluded on cost grounds instead: 2^12 = 4096 coalitions
# is more than twice the largest kernel budget there (20 blocks = 960 draws =
# 1922 evals), so this lane has no ground-truth arm on bike_sharing. friedman1
# (10 features, 1024 coalitions) is kept: that is below its own largest kernel
# budget (800 draws = 1602 evals), so it is a fair ground truth rather than an
# outlier expense.
exact_infeasible <- unwrap(getJobTable())[
	estimator == "exact" & problem == "bike_sharing",
]

if (nrow(exact_infeasible) > 0) {
	cli::cli_alert_warning(
		"Removing {nrow(exact_infeasible)} exact-estimator job(s) on tasks above max_features"
	)
	removeExperiments(exact_infeasible)
}

# ============================================================================
# Optional: Tag specific job combinations for analysis
# ============================================================================

# Tag real data comparison experiments
findExperiments(
	prob.name = c("bike_sharing")
) |>
	addJobTags(tags = "real_data")

# Provider tags are derived from the algorithm name by convention, so new
# algorithms are tagged automatically (xplainfi / reference, plus python for
# reticulate-backed reference implementations).
for (nm in active_algos) {
	tags <- algo_provider(nm)
	if (algo_is_python(nm)) {
		tags <- c(tags, "python")
	}
	findExperiments(algo.name = nm) |>
		addJobTags(tags = tags)
}

# ============================================================================
# Record provenance (xplainfi version + git SHA) for this registry
# ============================================================================

prov <- write_provenance(conf$reg_path, providers = conf$providers)
cli::cli_alert_info(
	"Provenance: xplainfi {.val {prov$xplainfi_version}} @ {.val {substr(prov$xplainfi_sha, 1, 10)}}"
)

# ============================================================================
# Experiment Summary
# ============================================================================

cli::cli_h1("Experiment Summary")

tab <- unwrap(getJobTable())
cli::cli_alert_info("Total jobs: {.strong {nrow(tab)}}")
cli::cli_alert_info("Problems: {.strong {length(prob_designs)}}")
cli::cli_alert_info("Algorithms: {.strong {length(algo_designs)}}")
cli::cli_alert_info("Replications: {.strong {conf$repls}}")

# Show job distribution
cli::cli_h2("Job Distribution by Problem and Algorithm")
job_dist <- unwrap(tab)[, .N, by = .(problem, algorithm)]
setorder(job_dist, problem, algorithm)
print(job_dist)

# Show parameter coverage
cli::cli_h2("Parameter Coverage")
cli::cli_ul(c(
	"Sample sizes: {paste(conf$n_samples, collapse = ', ')}",
	"Feature dimensions (peak task): {paste(conf$n_features, collapse = ', ')}",
	"Learner types: {paste(conf$learner_types, collapse = ', ')}",
	"n_repeats: {paste(conf$n_repeats, collapse = ', ')}",
	"n_permutations (SAGE): {paste(conf$n_permutations, collapse = ', ')}",
	"Samplers (CFI/ConditionalSAGE): {length(conf$samplers)}"
))

cli::cli_alert_success("Experiment registry created at: {.path {fs::path_rel(conf$reg_path)}}")
