# Algorithm definitions for batchtools experiment
library(batchtools)
library(xplainfi)
library(mlr3)
library(mlr3learners)
library(data.table)
library(here)

source(here::here("benchmark", "config.R"))

# Algorithm: Permutation Feature Importance (PFI)
addAlgorithm(
  name = "PFI",
  fun = function(data, job, instance, n_permutations, ...) {
    start_time <- Sys.time()

    # Create method
    pfi <- PFI$new(
      task = instance$task,
      learner = create_learner(num.trees = exp_settings$n_trees),
      measure = create_measure(),
      resampling = create_resampling(),
      iters_perm = n_permutations
    )

    # Compute importance
    importance <- pfi$compute()

    end_time <- Sys.time()
    runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Return results
    list(
      importance = importance,
      runtime = runtime,
      method = "PFI",
      n_permutations = n_permutations,
      task_info = list(
        task_type = instance$task_type,
        n_features = instance$n_features,
        n_samples = instance$n_samples
      )
    )
  }
)

# Algorithm: Conditional Feature Importance (CFI)
addAlgorithm(
  name = "CFI",
  fun = function(data, job, instance, n_permutations, ...) {
    start_time <- Sys.time()

    # Create method
    cfi <- CFI$new(
      task = instance$task,
      learner = create_learner(num.trees = exp_settings$n_trees),
      measure = create_measure(),
      resampling = create_resampling(),
      iters_perm = n_permutations
    )

    # Compute importance
    importance <- cfi$compute()

    end_time <- Sys.time()
    runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

    list(
      importance = importance,
      runtime = runtime,
      method = "CFI",
      n_permutations = n_permutations,
      task_info = list(
        task_type = instance$task_type,
        n_features = instance$n_features,
        n_samples = instance$n_samples
      )
    )
  }
)

# Algorithm: Relative Feature Importance (RFI)
addAlgorithm(
  name = "RFI",
  fun = function(data, job, instance, n_permutations, conditioning_features = NULL, ...) {
    start_time <- Sys.time()

    # Default conditioning set: first feature only
    if (is.null(conditioning_features)) {
      conditioning_features <- instance$task$feature_names[1]
    }

    # Create method
    rfi <- RFI$new(
      task = instance$task,
      learner = create_learner(num.trees = exp_settings$n_trees),
      measure = create_measure(),
      resampling = create_resampling(),
      iters_perm = n_permutations,
      conditioning_set = conditioning_features
    )

    # Compute importance
    importance <- rfi$compute()

    end_time <- Sys.time()
    runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

    list(
      importance = importance,
      runtime = runtime,
      method = "RFI",
      n_permutations = n_permutations,
      conditioning_set = conditioning_features,
      task_info = list(
        task_type = instance$task_type,
        n_features = instance$n_features,
        n_samples = instance$n_samples
      )
    )
  }
)

# Algorithm: Marginal SAGE
addAlgorithm(
  name = "MarginalSAGE",
  fun = function(data, job, instance, n_permutations, reference_proportion, ...) {
    start_time <- Sys.time()

    # Create method
    sage <- MarginalSAGE$new(
      task = instance$task,
      learner = create_learner(num.trees = exp_settings$n_trees),
      measure = create_measure(),
      resampling = create_resampling(),
      n_permutations = n_permutations,
      max_reference_size = floor(instance$n_samples * reference_proportion)
    )

    # Compute importance
    importance <- sage$compute()

    end_time <- Sys.time()
    runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

    list(
      importance = importance,
      runtime = runtime,
      method = "MarginalSAGE",
      n_permutations = n_permutations,
      reference_proportion = reference_proportion,
      task_info = list(
        task_type = instance$task_type,
        n_features = instance$n_features,
        n_samples = instance$n_samples
      )
    )
  }
)

# Algorithm: Conditional SAGE
addAlgorithm(
  name = "ConditionalSAGE",
  fun = function(data, job, instance, n_permutations, reference_proportion, ...) {
    # Skip if arf not available
    if (!requireNamespace("arf", quietly = TRUE)) {
      return(list(
        error = "arf package not available",
        method = "ConditionalSAGE"
      ))
    }

    start_time <- Sys.time()

    # Create method
    sage <- ConditionalSAGE$new(
      task = instance$task,
      learner = create_learner(num.trees = exp_settings$n_trees),
      measure = create_measure(),
      resampling = create_resampling(),
      n_permutations = n_permutations,
      max_reference_size = floor(instance$n_samples * reference_proportion)
    )

    # Compute importance
    importance <- sage$compute()

    end_time <- Sys.time()
    runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

    list(
      importance = importance,
      runtime = runtime,
      method = "ConditionalSAGE",
      n_permutations = n_permutations,
      reference_proportion = reference_proportion,
      task_info = list(
        task_type = instance$task_type,
        n_features = instance$n_features,
        n_samples = instance$n_samples
      )
    )
  }
)

# Algorithm: LOCO
addAlgorithm(
  name = "LOCO",
  fun = function(data, job, instance, n_refits, ...) {
    start_time <- Sys.time()

    # Create method
    loco <- LOCO$new(
      task = instance$task,
      learner = create_learner(num.trees = exp_settings$n_trees),
      measure = create_measure(),
      resampling = create_resampling(),
      iters_refit = n_refits
    )

    # Compute importance
    importance <- loco$compute()

    end_time <- Sys.time()
    runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

    list(
      importance = importance,
      runtime = runtime,
      method = "LOCO",
      task_info = list(
        task_type = instance$task_type,
        n_features = instance$n_features,
        n_samples = instance$n_samples
      )
    )
  }
)

# Algorithm: LOCI
addAlgorithm(
  name = "LOCI",
  fun = function(data, job, instance, n_refits, ...) {
    start_time <- Sys.time()

    # Create method
    loci <- LOCI$new(
      task = instance$task,
      learner = create_learner(num.trees = exp_settings$n_trees),
      measure = create_measure(),
      resampling = create_resampling(),
      iters_refit = n_refits
    )

    # Compute importance
    importance <- loci$compute()

    end_time <- Sys.time()
    runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

    list(
      importance = importance,
      runtime = runtime,
      method = "LOCI",
      task_info = list(
        task_type = instance$task_type,
        n_features = instance$n_features,
        n_samples = instance$n_samples
      )
    )
  }
)
