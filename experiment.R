# Main experiment setup and execution script
library(batchtools)
library(data.table)

# Load configuration
source(here::here("benchmark", "config.R"))

# Create or load registry
reg_path <- here::here("benchmark", "registry")
unlink(reg_path, recursive = TRUE)
if (dir.exists(reg_path)) {
  reg <- loadRegistry(reg_path, writeable = TRUE)
} else {
  reg <- makeExperimentRegistry(
    file.dir = reg_path,
    packages = packages,
    seed = exp_settings$seed,
    source = here::here("benchmark", c("helpers.R", "config.R"))
  )
}

# Load problems and algorithms
source(here::here("benchmark", "helpers.R"))
source(here::here("benchmark", "problems.R"))
source(here::here("benchmark", "algorithms.R"))

# Define experiment design
prob_designs <- list(
  # Friedman1 with fixed 10 features, varying sample sizes
  friedman1 = data.table(
    n_samples = exp_settings$n_samples
  ),

  # Peak with varying dimensions and sample sizes
  peak = data.table(
    n_samples = rep(exp_settings$n_samples, each = 4),
    d_features = rep(c(5, 10, 15, 20), times = length(exp_settings$n_samples))
  ),

  # Bike sharing (real-world data, fixed dimensions)
  bike_sharing = data.table(
    n_samples = exp_settings$n_samples
  )
)

# Algorithm designs
algo_designs <- list(
  # Permutation-based methods
  PFI = data.frame(n_permutations = exp_settings$n_permutations),
  CFI = data.frame(n_permutations = exp_settings$n_permutations),
  RFI = data.frame(n_permutations = exp_settings$n_permutations),
  MarginalSAGE = expand.grid(
    n_permutations = exp_settings$n_permutations,
    reference_proportion = exp_settings$reference_proportions
  ),
  ConditionalSAGE = expand.grid(
    n_permutations = exp_settings$n_permutations,
    reference_proportion = exp_settings$reference_proportions
  ),
  # Leave-one-out methods (no permutations)
  LOCO = data.frame(n_refits = exp_settings$n_refits),
  LOCI = data.frame(n_refits = exp_settings$n_refits)
)

# Add experiments to registry
addExperiments(
  prob.designs = prob_designs,
  algo.designs = algo_designs,
  repls = 3 # 3 replications for each configuration
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
