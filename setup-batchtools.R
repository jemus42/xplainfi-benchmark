# Main experiment setup and execution script
# Problem definitions for batchtools experiment
library(batchtools)
library(mlr3)
library(data.table)

# Load configuration
source(here::here("config.R"))

stopifnot(
  "Not all packages installed" = all(sapply(
    packages,
    requireNamespace,
    quietly = TRUE
  ))
)

# Create or load registry
if (dir.exists(reg_path)) {
  cli::cli_alert_danger("Deleting existing registry at {.file {reg_path}}")
  fs::dir_delete(reg_path)
}

cli::cli_alert_info("Creating registry at {.file {reg_path}}")
reg <- makeExperimentRegistry(
  file.dir = reg_path,
  packages = c("mlr3learners", "xplainfi"),
  seed = exp_settings$seed,
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
# Problem Designs
# ============================================================================

prob_designs <- list(
  # Friedman1: fixed 10 features, varying sample sizes
  friedman1 = CJ(
    n_samples = exp_settings$n_samples,
    learner_type = exp_settings$learner_types
  ),

  # Peak: varying dimensions and sample sizes
  peak = CJ(
    n_samples = exp_settings$n_samples,
    n_features = exp_settings$n_features,
    learner_type = exp_settings$learner_types
  ),

  # Bike sharing: real-world data, fixed dimensions
  bike_sharing = CJ(
    n_samples = exp_settings$n_samples,
    learner_type = exp_settings$learner_types
  ),

  # Correlated features DGP: varying correlation strength
  correlated = CJ(
    n_samples = exp_settings$n_samples,
    correlation = exp_settings$correlation,
    learner_type = exp_settings$learner_types
  ),

  # Ewald DGP: fixed structure
  ewald = CJ(
    n_samples = exp_settings$n_samples,
    learner_type = exp_settings$learner_types
  ),

  # Interactions DGP: fixed structure
  interactions = CJ(
    n_samples = exp_settings$n_samples,
    learner_type = exp_settings$learner_types
  )
)

# ============================================================================
# Algorithm Designs
# ============================================================================

algo_designs <- list(
  # PFI: Permutation Feature Importance
  PFI = data.table(
    n_repeats = exp_settings$n_repeats
  ),

  # CFI: Conditional Feature Importance (with samplers)
  CFI = CJ(
    n_repeats = exp_settings$n_repeats,
    sampler = exp_settings$samplers
  ),

  # RFI: Relative Feature Importance (with samplers)
  RFI = CJ(
    n_repeats = exp_settings$n_repeats,
    sampler = exp_settings$samplers
  ),

  # LOCO: Leave-One-Covariate-Out
  LOCO = data.table(
    n_repeats = exp_settings$n_repeats
  ),

  # MarginalSAGE
  MarginalSAGE = CJ(
    n_permutations = exp_settings$n_permutations,
    sage_n_samples = exp_settings$sage_n_samples
  ),

  # ConditionalSAGE (with samplers)
  ConditionalSAGE = CJ(
    n_permutations = exp_settings$n_permutations,
    sage_n_samples = exp_settings$sage_n_samples,
    sampler = exp_settings$samplers
  ),

  # PFI_mlr3filters: Reference implementation from mlr3filters
  # PFI_mlr3filters = CJ(
  #   n_repeats = exp_settings$n_repeats
  # ),

  # PFI_iml: Reference implementation from iml package
  PFI_iml = data.table(
    n_repeats = exp_settings$n_repeats
  ),

  # PFI_vip: Reference implementation from vip package
  PFI_vip = data.table(
    n_repeats = exp_settings$n_repeats
  ),

  # PFI_fippy: Reference implementation from fippy package (Python)
  PFI_fippy = data.table(
    n_repeats = exp_settings$n_repeats
  ),

  # CFI_fippy: Conditional FI from fippy package (Python, Gaussian sampler)
  CFI_fippy = data.table(
    n_repeats = exp_settings$n_repeats
  ),

  # MarginalSAGE_fippy: Marginal SAGE from fippy package (Python)
  MarginalSAGE_fippy = data.table(
    n_permutations = exp_settings$n_permutations,
    sage_n_samples = exp_settings$sage_n_samples
  ),

  # ConditionalSAGE_fippy: Conditional SAGE from fippy package (Python)
  ConditionalSAGE_fippy = data.table(
    n_permutations = exp_settings$n_permutations,
    sage_n_samples = exp_settings$sage_n_samples
  ),

  # KernelSAGE: Official SAGE implementation with kernel estimator
  KernelSAGE = data.table(
    sage_n_samples = exp_settings$sage_n_samples
  )
)

# ============================================================================
# Add Experiments
# ============================================================================

cli::cli_h1("Adding Experiments to Registry")

addExperiments(
  prob.designs = prob_designs,
  algo.designs = algo_designs,
  repls = exp_settings$repls
)

# ============================================================================
# Remove incompatible sampler-task combinations
# ============================================================================

# Gaussian sampler doesn't support mixed feature types (bike_sharing)
# sampler is NA for most jobs, findExperiments() didn't play nice
incompatible_jobs = unwrap(getJobTable())[!is.na(sampler)][
  (sampler == "gaussian") & problem == "bike_sharing",
]

if (nrow(incompatible_jobs) > 0) {
  cli::cli_alert_warning(
    "Removing {nrow(incompatible_jobs)} incompatible job(s): bike_sharing × gaussian sampler"
  )
  removeExperiments(incompatible_jobs)
}

# ============================================================================
# Optional: Tag specific job combinations for analysis
# ============================================================================

# Tag runtime benchmark jobs (e.g., featureless learner on peak)
findExperiments(
  prob.pars = learner_type == "featureless",
  prob.name = "peak"
) |>
  addJobTags(tags = "runtime")

# Tag DGP comparison experiments
mlr3misc::walk(c("ewald", "interactions", "correlated", "friedman1"), \(x) {
  findExperiments(
    prob.name = x
  ) |>
    addJobTags(tags = "dgp_comparison")
})

# Tag real data comparison experiments
findExperiments(
  prob.name = c("bike_sharing")
) |>
  addJobTags(tags = "real_data")


# ============================================================================
# Experiment Summary
# ============================================================================

cli::cli_h1("Experiment Summary")

job_table <- getJobTable()
cli::cli_alert_info("Total jobs: {.strong {nrow(job_table)}}")
cli::cli_alert_info("Problems: {.strong {length(prob_designs)}}")
cli::cli_alert_info("Algorithms: {.strong {length(algo_designs)}}")
cli::cli_alert_info("Replications: {.strong {exp_settings$repls}}")

# Show job distribution
cli::cli_h2("Job Distribution by Problem and Algorithm")
job_dist <- unwrap(job_table)[, .N, by = .(problem, algorithm)]
setorder(job_dist, problem, algorithm)
print(job_dist)

# Show parameter coverage
cli::cli_h2("Parameter Coverage")
cli::cli_ul(c(
  "Sample sizes: {paste(exp_settings$n_samples, collapse = ', ')}",
  "Feature dimensions (peak task): {paste(exp_settings$n_features, collapse = ', ')}",
  "Learner types: {paste(exp_settings$learner_types, collapse = ', ')}",
  "n_repeats: {paste(exp_settings$n_repeats, collapse = ', ')}",
  "n_permutations (SAGE): {paste(exp_settings$n_permutations, collapse = ', ')}",
  "Samplers (CFI/RFI/ConditionalSAGE): {length(exp_settings$samplers)}"
))

cli::cli_alert_success("Experiment registry created at: {.path {reg_path}}")
cli::cli_alert_info("Next steps:")
cli::cli_ul(c(
  "Run jobs: source('run_experiment.R')",
  "Collect results: source('collect_results.R')"
))
