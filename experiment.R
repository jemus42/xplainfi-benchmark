# Main experiment setup and execution script
library(batchtools)
library(data.table)

# Load configuration
source(here::here("config.R"))

stopifnot(
  "Not all packages installed" = all(sapply(packages, requireNamespace))
)

# Create or load registry
reg_path <- here::here("registry")
unlink(reg_path, recursive = TRUE)
if (dir.exists(reg_path)) {
  # reg <- loadRegistry(reg_path, writeable = TRUE)
  fs::dir_delete(reg_path)
} else {
  reg <- makeExperimentRegistry(
    file.dir = reg_path,
    packages = c("mlr3", "xplainfi"),
    seed = exp_settings$seed,
    source = here::here(c("helpers.R", "config.R"))
  )
}

# Load problems and algorithms
source(here::here("helpers.R"))
source(here::here("problems.R"))
source(here::here("algorithms.R"))

# Define experiment design
prob_designs <- list(
  # Friedman1 with fixed 10 features, varying sample sizes
  friedman1 = data.table(
    n_samples = exp_settings$n_samples
  ),

  # Peak with varying dimensions and sample sizes
  peak = expand.grid(
    n_samples = exp_settings$n_samples,
    n_features = c(5, 10, 20, 50)
  ),

  # Bike sharing (real-world data, fixed dimensions)
  bike_sharing = data.table(
    n_samples = exp_settings$n_samples
  )
)


# Algorithm designs
algo_designs <- list(
  # Permutation-based methods
  PFI = expand.grid(
    n_permutations = exp_settings$n_permutations,
    learner_type = exp_settings$learner_types,
    n_trees = exp_settings$n_trees,
    stringsAsFactors = FALSE
  ),
  CFI = expand.grid(
    n_permutations = exp_settings$n_permutations,
    learner_type = exp_settings$learner_types,
    n_trees = exp_settings$n_trees,
    stringsAsFactors = FALSE
  ),
  RFI = expand.grid(
    n_permutations = exp_settings$n_permutations,
    learner_type = exp_settings$learner_types,
    n_trees = exp_settings$n_trees,
    stringsAsFactors = FALSE
  ),
  MarginalSAGE = expand.grid(
    n_permutations = exp_settings$n_permutations,
    reference_proportion = exp_settings$reference_proportions,
    learner_type = exp_settings$learner_types,
    n_trees = exp_settings$n_trees,
    stringsAsFactors = FALSE
  ),
  ConditionalSAGE = expand.grid(
    n_permutations = exp_settings$n_permutations,
    reference_proportion = exp_settings$reference_proportions,
    learner_type = exp_settings$learner_types,
    n_trees = exp_settings$n_trees,
    stringsAsFactors = FALSE
  ),
  LOCO = expand.grid(
    n_refits = exp_settings$n_refits,
    learner_type = exp_settings$learner_types,
    n_trees = exp_settings$n_trees,
    stringsAsFactors = FALSE
  )
)

# Add experiments to registry
addExperiments(
  prob.designs = prob_designs,
  algo.designs = algo_designs,
  repls = 10
)

# Summary of experiments
cli::cli_h1("Experiment Summary")
cli::cli_alert_info("Total jobs: {nrow(getJobTable())}")
cli::cli_alert_info("Problems: {length(prob_designs)}")
cli::cli_alert_info("Algorithms: {length(algo_designs)}")

# Show job table
cli::cli_h2("Job Distribution")
tab <- unwrap(getJobTable())
print(tab[, .N, by = .(problem, algorithm)])

cli::cli_alert_success("Experiment registry created at: {.path {reg_path}}")
