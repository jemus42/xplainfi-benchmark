# Main experiment setup and execution script
library(batchtools)
library(data.table)
library(here)

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
  source = here::here(c("helpers.R", "config.R"))
)

# Load problems and algorithms
source(here::here("helpers.R"))
source(here::here("problems.R"))
source(here::here("algorithms.R"))

# ============================================================================
# Problem Designs
# ============================================================================

prob_designs <- list(
  # Friedman1: fixed 10 features, varying sample sizes
  friedman1 = data.table(
    n_samples = exp_settings$n_samples
  ),

  # Peak: varying dimensions and sample sizes
  peak = CJ(
    n_samples = exp_settings$n_samples,
    n_features = exp_settings$n_features
  ),

  # Bike sharing: real-world data, fixed dimensions
  bike_sharing = data.table(
    n_samples = exp_settings$n_samples
  ),

  # Correlated features DGP: varying correlation strength
  correlated = CJ(
    n_samples = exp_settings$n_samples,
    correlation = c(0.5, 0.75, 0.9)
  ),

  # Ewald DGP: fixed structure
  ewald = data.table(
    n_samples = exp_settings$n_samples
  ),

  # Interactions DGP: fixed structure
  interactions = data.table(
    n_samples = exp_settings$n_samples
  )
)

# ============================================================================
# Algorithm Designs
# ============================================================================

algo_designs <- list(
  # PFI: Permutation Feature Importance
  PFI = CJ(
    n_repeats = exp_settings$n_repeats,
    learner_type = exp_settings$learner_types
  ),

  # CFI: Conditional Feature Importance (with samplers)
  CFI = CJ(
    n_repeats = exp_settings$n_repeats,
    sampler = exp_settings$samplers,
    learner_type = exp_settings$learner_types
  ),

  # RFI: Relative Feature Importance (with samplers)
  RFI = CJ(
    n_repeats = exp_settings$n_repeats,
    sampler = exp_settings$samplers,
    learner_type = exp_settings$learner_types
  ),

  # LOCO: Leave-One-Covariate-Out
  LOCO = CJ(
    n_repeats = exp_settings$n_repeats,
    learner_type = exp_settings$learner_types
  ),

  # MarginalSAGE
  MarginalSAGE = CJ(
    n_permutations = exp_settings$n_permutations,
    sage_n_samples = exp_settings$sage_n_samples,
    learner_type = exp_settings$learner_types
  ),

  # ConditionalSAGE (with samplers)
  ConditionalSAGE = CJ(
    n_permutations = exp_settings$n_permutations,
    sage_n_samples = exp_settings$sage_n_samples,
    sampler = exp_settings$samplers,
    learner_type = exp_settings$learner_types
  ),

  # PFI_mlr3filters: Reference implementation from mlr3filters
  PFI_mlr3filters = CJ(
    n_repeats = exp_settings$n_repeats,
    learner_type = exp_settings$learner_types
  ),

  # PFI_iml: Reference implementation from iml package
  PFI_iml = CJ(
    n_repeats = exp_settings$n_repeats,
    learner_type = exp_settings$learner_types
  ),

  # PFI_vip: Reference implementation from vip package
  PFI_vip = CJ(
    n_repeats = exp_settings$n_repeats,
    learner_type = exp_settings$learner_types
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
# Optional: Tag specific job combinations for analysis
# ============================================================================

# Tag runtime benchmark jobs (e.g., featureless learner on peak)
findExperiments(
  algo.pars = learner_type == "featureless",
  prob.name = "peak"
) |>
  addJobTags(tags = "runtime_benchmark")

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
  "Feature dimensions (peak): {paste(exp_settings$n_features, collapse = ', ')}",
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
