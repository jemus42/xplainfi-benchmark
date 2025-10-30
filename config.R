# Configuration file for batchtools experiment
# Registry configuration
reg_path <- fs::path(
  here::here("registries"),
  paste0("xplainfi-", packageVersion("xplainfi"))
)
if (!dir.exists(here::here("registries"))) {
  dir.create(here::here("registries"))
}

# Ensure ranger behaves, particularly important for nested parallelization here with conditional sampling depending on ranger as well
options(ranger.num.threads = 1)
data.table::setDTthreads(1)
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OMP_THREAD_LIMIT = 1)
Sys.setenv(MKL_NUM_THREADS = 1)

# Package dependencies, will be checked for installation
packages <- c(
  "xplainfi",
  "mlr3",
  "mlr3learners",
  # "mlr3filters",
  "mlr3pipelines",
  "mlbench",
  "batchtools",
  "data.table",
  "checkmate",
  "digest",
  "iml",
  "vip",
  "nnet",
  "arf",
  "partykit",
  "mvtnorm"
)

# Experiment settings
exp_settings <- list(
  # General batchtools settings
  seed = 2025,
  repls = 10,
  # Samples to generate or to subsample real data to (bike_sharing)
  n_samples = c(100, 500, 1000),
  # Only one task with variable number of features
  n_features = c(5, 10, 50),
  # Affects correlation task
  correlation = c(0.5, 0.9),
  # Affects PFI, CFI, RFI, and LOCO iterations
  n_repeats = c(1, 10, 100),
  # For SAGE permutations
  n_permutations = c(5, 10, 30),
  # Size of sampled data used for Monte Carlo integration in SAGE methods
  sage_n_samples = 200L,
  # Types of learners to use for each method, uses create_learner helper
  learner_types = c("featureless", "linear", "ranger", "nnet"),
  # Fixed number of trees for ranger (not varied in experiments)
  n_trees = 500L,
  # Conditional samplers for CFI, RFI, and ConditionalSAGE
  samplers = c(
    "arf",
    "gaussian",
    "knn",
    "ctree"
  )
)
