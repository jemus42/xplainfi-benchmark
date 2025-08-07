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
  n_samples = c(100, 500, 1000),
  n_permutations = c(1, 5, 10, 50),
  n_refits = c(1, 5, 10, 50),
  reference_proportions = c(0.1, 0.3, 0.5),
  learner_types = c("featureless", "linear", "ranger"),
  n_trees = c(100, 500),
  seed = 2025
)
