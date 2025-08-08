# Configuration file for batchtools experiment
# Registry configuration
reg_path <- here::here("registry")

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
  "mlbench",
  "data.table",
  "checkmate",
  "arf"
)

# Experiment settings
exp_settings <- list(
  # General batchtools settings
  seed = 2025,
  repls = 3,
  # Samples to generate or to subsample real data to (bike_sharing)
  n_samples = c(100, 500, 1000),
  # Only one task with variable number of features
  n_features = c(5, 10, 50),
  # Affects P|C|RFI iterations and SAGE permutations
  n_permutations = c(1, 5, 10, 50),
  # LOCO refits
  n_refits = c(1, 5, 10, 50),
  # Size of reference dataset in SAGE impls
  reference_proportions = c(0.1, 0.3),
  # Types of learners to use for each method, uses create_learner helper
  learner_types = c("featureless", "linear", "ranger"),
  # Only relevant for ranger
  n_trees = 500L
)
