# Configuration file for batchtools experiment
library(here)

# Registry configuration
reg_path <- here::here("benchmark", "registry")

# Cluster configuration for local execution
# cluster.functions <- batchtools::makeClusterFunctionsInteractive()

# Resource defaults
default.resources <- list(
  walltime = 60 * 60, # 1 hour
  memory = 4096, # 4 GB
  ncpus = 1
)

# Ensure ranger behaves, particularly important for nested parallelization here with conditional sampling depending on ranger as well
options(ranger.num.threads = 1)
data.table::setDTthreads(1)

# Package dependencies
packages <- c(
  "xplainfi",
  "mlr3",
  "mlr3learners",
  "mlbench",
  "data.table",
  "checkmate"
)

# Experiment settings
exp_settings <- list(
  n_samples = c(100, 500, 1000),
  n_permutations = c(1, 5, 10),
  n_refits = c(1, 5, 10),
  reference_proportions = c(0.1, 0.3, 0.5),
  n_trees = 100,
  seed = 2025
)
