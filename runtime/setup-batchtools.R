# Main experiment setup and execution script
# Problem definitions for batchtools experiment

# Load configuration
source(here::here("setup-common.R"))
source(here::here("runtime", "config.R"))

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
		source = here::here(c("R/helpers.R", "R/helpers-python.R", "runtime/config.R"))
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

addProblem(name = "peak", data = NULL, fun = prob_peak, seed = conf$seed)

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
	# Peak: varying dimensions and sample sizes
	peak = CJ(
		n_samples = conf$n_samples,
		n_features = conf$n_features,
		learner_type = conf$learner_types
	)
)

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
	MarginalSAGE_sage = sage_algo_design(
		conf,
		estimators = c("permutation", "kernel"),
		kernel_variants = NA_character_
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
	PFI_fippy = data.table(
		n_repeats = conf$n_repeats,
		sampler = "simple"
	),

	# CFI_fippy: Conditional FI from fippy package (Python, Gaussian sampler)
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

# Featureless learner is only used for xplainfi runtime benchmarking
featureless_non_xplainfi_jobs <- unwrap(getJobTable())[
	learner_type == "featureless" &
		!(algorithm %in% c("PFI", "CFI", "MarginalSAGE", "ConditionalSAGE", "LOCO")),
]

if (nrow(featureless_non_xplainfi_jobs) > 0) {
	cli::cli_alert_warning(
		"Removing {nrow(featureless_non_xplainfi_jobs)} job(s) for other methods with featureless learner"
	)
	removeExperiments(featureless_non_xplainfi_jobs)
}

# ============================================================================
# Remove infeasible exact-estimator jobs
# ============================================================================

# estimator = "exact" enumerates 2^n_features coalitions and aborts above
# max_features (12L). Keeps n_features 5 (32 coalitions) and 10 (1024).
exact_infeasible <- unwrap(getJobTable())[
	estimator == "exact" & n_features > 12,
]

if (nrow(exact_infeasible) > 0) {
	cli::cli_alert_warning(
		"Removing {nrow(exact_infeasible)} exact-estimator job(s) above max_features"
	)
	removeExperiments(exact_infeasible)
}

# for SAGE with early stopping we only keep maximum n_permutations
# Scoped to the permutation estimator: kernel/exact rows carry NA budgets, and
# early_stopping is permutation-only. (With sage_early_stopping = FALSE this is
# a no-op, but leaving it unscoped is a trap if early stopping is turned back on.)
sage_early_stopping <- unwrap(getJobTable())[
	estimator == "permutation" &
		early_stopping &
		n_permutations < max(n_permutations, na.rm = TRUE),
]
# sage_early_stopping[, .N, by = c("early_stopping", "n_permutations", "algorithm")]

if (nrow(sage_early_stopping) > 0) {
	cli::cli_alert_warning(
		"Removing {nrow(sage_early_stopping)} job(s) for other SAGE with early stopping and lower n_permutations"
	)
	removeExperiments(sage_early_stopping)
}


# ============================================================================
# Optional: Tag specific job combinations for analysis
# ============================================================================

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
