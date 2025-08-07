# Problem definitions for batchtools experiment
library(batchtools)
library(mlr3)
library(mlbench)
library(data.table)
library(here)

source(here::here("config.R"))

# Problem: Friedman1 regression task (fixed 10 features)
addProblem(
  name = "friedman1",
  data = NULL,
  fun = function(data, job, n_samples, ...) {
    # Use mlr3 task generator for Friedman1
    task <- tgen("friedman1")$generate(n = n_samples)
    
    list(
      task = task,
      n_features = length(task$feature_names), # Friedman1 always has 10 features
      n_samples = task$nrow,
      task_type = "friedman1"
    )
  },
  seed = exp_settings$seed
)

# Problem: Peak regression task
addProblem(
  name = "peak",
  data = NULL,
  fun = function(data, job, n_samples, n_features, ...) {
    # Generate peak data
    data <- mlbench::mlbench.peak(n = n_samples, d = n_features)

    # Create data.frame
    df <- data.frame(data$x, y = data$y)
    colnames(df) <- c(paste0("x", seq_len(n_features)), "y")

    # Create mlr3 task
    task <- TaskRegr$new(
      id = paste0("peak_d", n_features, "_n", n_samples),
      backend = df,
      target = "y"
    )

    list(
      task = task,
      n_features = n_features,
      n_samples = n_samples,
      task_type = "peak"
    )
  },
  seed = exp_settings$seed
)

# Problem: Bike Sharing (real-world regression task)
addProblem(
  name = "bike_sharing",
  data = NULL,
  fun = function(data, job, n_samples, ...) {
    # Load bike sharing task (requires mlr3data)
    if (!requireNamespace("mlr3data", quietly = TRUE)) {
      stop("mlr3data package required for bike_sharing task")
    }

    library(mlr3data)
    task <- tsk("bike_sharing")

    # Subsample if requested
    if (n_samples < task$nrow) {
      sample_ids <- sample(task$row_ids, size = n_samples)
      task <- task$clone()$filter(sample_ids)
    }

    list(
      task = task,
      n_features = length(task$feature_names),
      n_samples = task$nrow,
      task_type = "bike_sharing"
    )
  },
  seed = exp_settings$seed
)
