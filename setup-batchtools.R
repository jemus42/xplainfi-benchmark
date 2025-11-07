# Main experiment setup and execution script
# Problem definitions for batchtools experiment
library(batchtools)
library(mlr3)
library(data.table)

# Load configuration
source(here::here("setup-common.R"))
source(here::here("config.R"))


# Create or load registry
if (dir.exists(conf$reg_path)) {
	cli::cli_alert_danger("Deleting existing registry at {.file {fs::path_rel(conf$reg_path)}}")
	fs::dir_delete(conf$reg_path)
}

cli::cli_alert_info("Creating registry at {.file {fs::path_rel(conf$reg_path)}}")
reg <- makeExperimentRegistry(
	file.dir = conf$reg_path,
	packages = c("mlr3learners", "xplainfi"),
	seed = conf$seed,
	source = here::here(c("R/helpers.R", "R/helpers-python.R", "config.R"))
)


# Load problems and algorithms
# mlr3misc::walk(
#   list.files(here::here("R"), pattern = "*.R", full.names = TRUE),
#   source,
#   echo = FALSE,
#   verbose = FALSE
# )
source(here::here("R/helpers.R"))
source(here::here("R/helpers-python.R"))
source(here::here("R/problems.R"))
source(here::here("R/algorithms.R"))

# ============================================================================
# Register Problems with batchtools
# ============================================================================

addProblem(name = "ewald", data = NULL, fun = prob_ewald, seed = conf$seed)
addProblem(name = "correlated", data = NULL, fun = prob_correlated, seed = conf$seed)
addProblem(name = "interactions", data = NULL, fun = prob_interactions, seed = conf$seed)
addProblem(name = "bike_sharing", data = NULL, fun = prob_bike_sharing, seed = conf$seed)
addProblem(name = "friedman1", data = NULL, fun = prob_friedman1, seed = conf$seed)
addProblem(name = "independent", data = NULL, fun = prob_independent, seed = conf$seed)
addProblem(name = "confounded", data = NULL, fun = prob_confounded, seed = conf$seed)
addProblem(name = "mediated", data = NULL, fun = prob_mediated, seed = conf$seed)

# ============================================================================
# Register Algorithms with batchtools
# ============================================================================

addAlgorithm(name = "PFI", fun = algo_PFI)
addAlgorithm(name = "CFI", fun = algo_CFI)
# addAlgorithm(name = "RFI", fun = algo_RFI)
addAlgorithm(name = "LOCO", fun = algo_LOCO)
addAlgorithm(name = "MarginalSAGE", fun = algo_MarginalSAGE)
addAlgorithm(name = "ConditionalSAGE", fun = algo_ConditionalSAGE)
addAlgorithm(name = "PFI_iml", fun = algo_PFI_iml)
addAlgorithm(name = "PFI_vip", fun = algo_PFI_vip)
addAlgorithm(name = "PFI_fippy", fun = algo_PFI_fippy)
addAlgorithm(name = "CFI_fippy", fun = algo_CFI_fippy)
addAlgorithm(name = "MarginalSAGE_fippy", fun = algo_MarginalSAGE_fippy)
addAlgorithm(name = "ConditionalSAGE_fippy", fun = algo_ConditionalSAGE_fippy)
addAlgorithm(name = "MarginalSAGE_sage", fun = algo_MarginalSAGE_sage)

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
		learner_type = conf$learner_types
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

	# RFI: Relative Feature Importance (with samplers)
	# RFI = CJ(
	# 	n_repeats = conf$n_repeats,
	# 	sampler = conf$samplers
	# ),

	# LOCO: Leave-One-Covariate-Out
	LOCO = data.table(
		n_repeats = conf$n_repeats
	),

	# MarginalSAGE
	MarginalSAGE = CJ(
		n_permutations = conf$n_permutations,
		sage_n_samples = conf$sage_n_samples,
		early_stopping = conf$sage_early_stopping
	),

	# ConditionalSAGE (with samplers)
	ConditionalSAGE = CJ(
		n_permutations = conf$n_permutations,
		sage_n_samples = conf$sage_n_samples,
		early_stopping = conf$sage_early_stopping,
		sampler = conf$samplers
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
		n_repeats = conf$n_repeats
	),

	# CFI_fippy: Conditional FI from fippy package (Python, Gaussian sampler)
	CFI_fippy = CJ(
		n_repeats = conf$n_repeats,
		sampler = "gaussian"
	),

	# MarginalSAGE_fippy: Marginal SAGE from fippy package (Python)
	MarginalSAGE_fippy = CJ(
		n_permutations = conf$n_permutations,
		sage_n_samples = conf$sage_n_samples,
		early_stopping = conf$sage_early_stopping
	),

	# ConditionalSAGE_fippy: Conditional SAGE from fippy package (Python)
	ConditionalSAGE_fippy = CJ(
		n_permutations = conf$n_permutations,
		sage_n_samples = conf$sage_n_samples,
		early_stopping = conf$sage_early_stopping,
		sampler = "gaussian"
	),

	# Kernel SAGE: Official SAGE implementation with kernel estimator
	MarginalSAGE_sage = data.table(
		sage_n_samples = conf$sage_n_samples,
		early_stopping = conf$sage_early_stopping
	)
)

# ============================================================================
# Add Experiments
# ============================================================================

cli::cli_h1("Adding Experiments to Registry")

addExperiments(
	prob.designs = prob_designs,
	algo.designs = algo_designs,
	repls = conf$repls
)

# ============================================================================
# Remove incompatible sampler-task combinations
# ============================================================================

# Gaussian sampler doesn't support mixed feature types (bike_sharing)
# sampler is NA for most jobs, findExperiments() didn't play nice
incompatible_jobs = unwrap(getJobTable())[!is.na(sampler)][
	(sampler %in% c("gaussian", "ctree")) & problem == "bike_sharing",
]

if (nrow(incompatible_jobs) > 0) {
	cli::cli_alert_warning(
		"Removing {nrow(incompatible_jobs)} incompatible job(s): bike_sharing × gaussian sampler"
	)
	removeExperiments(incompatible_jobs)
}

# Featureless learner is only used for xplainfi runtime benchmarking
featureless_non_xplainfi_jobs = unwrap(getJobTable())[
	learner_type == "featureless" &
		algorithm %in% c("PFI", "CFI", "RFI", "MarginalSAGE", "ConditionalSAGE", "LOCO"),
]

if (nrow(featureless_non_xplainfi_jobs) > 0) {
	cli::cli_alert_warning(
		"Removing {nrow(featureless_non_xplainfi_jobs)} job(s) for other methods with featureless learner"
	)
	removeExperiments(featureless_non_xplainfi_jobs)
}

# ============================================================================
# Optional: Tag specific job combinations for analysis
# ============================================================================

# Tag real data comparison experiments
findExperiments(
	prob.name = c("bike_sharing")
) |>
	addJobTags(tags = "real_data")

findExperiments(algo.pattern = "_fippy") |>
	addJobTags(tags = "python")

findExperiments(algo.pattern = "_sage") |>
	addJobTags(tags = "python")

# Explictly tag xplainfi jobs
for (algo in c("PFI", "CFI", "RFI", "MarginalSAGE", "ConditionalSAGE", "LOCO")) {
	findExperiments(algo.name = algo) |>
		addJobTags(tags = "xplainfi")
}

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
	"Samplers (CFI/RFI/ConditionalSAGE): {length(conf$samplers)}"
))

cli::cli_alert_success("Experiment registry created at: {.path {fs::path_rel(conf$reg_path)}}")
