# Algorithm definitions for batchtools experiment
library(batchtools)
library(xplainfi)
library(mlr3)
library(mlr3learners)
library(data.table)

source(here::here("config.R"))

# Generic algorithm factory to reduce code duplication
create_fi_algorithm <- function(
  method_name,
  method_class
) {
  addAlgorithm(
    name = method_name,
    fun = function(
      data,
      job,
      instance,
      learner_type = "ranger",
      resampling_type = "holdout",
      n_trees = 100,
      ...
    ) {
      # Detect task type
      task_type <- instance$task$task_type

      # Create learner
      learner <- create_learner(
        learner_type = learner_type,
        num.trees = n_trees,
        task_type = task_type
      )

      # Create measure
      measure <- create_measure(task_type = task_type)

      # Create resampling
      resampling <- create_resampling(type = resampling_type)

      # Extract method-specific parameters
      dots <- list(...)
      method_params <- list(
        task = instance$task,
        learner = learner,
        measure = measure,
        resampling = resampling
      )

      # Add method-specific parameters
      if ("n_permutations" %in% names(dots)) {
        # SAGE methods use n_permutations, PFI uses iters_perm
        if (method_name %in% c("MarginalSAGE", "ConditionalSAGE")) {
          method_params$n_permutations <- dots$n_permutations
        } else {
          method_params$iters_perm <- dots$n_permutations
        }
      }
      if ("n_refits" %in% names(dots)) {
        method_params$iters_refit <- dots$n_refits
      }
      if ("reference_proportion" %in% names(dots)) {
        method_params$max_reference_size <- floor(
          instance$n_samples * dots$reference_proportion
        )
      }
      if ("conditioning_features" %in% names(dots)) {
        method_params$conditioning_set <- dots$conditioning_features
      }

      # Handle RFI default conditioning set
      if (method_name == "RFI" && is.null(dots$conditioning_features)) {
        method_params$conditioning_set <- instance$task$feature_names[1]
      }

      # Create method instance
      method_instance <- do.call(method_class$new, method_params)

      # Compute importance
      start_time <- Sys.time()
      method_instance$compute()
      end_time <- Sys.time()
      runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

      # Create result list
      result <- list(
        importance = method_instance$importance(),
        scores = method_instance$scores,
        runtime = runtime
        # method = method_name,
        # learner_type = learner_type,
        # task_info = list(
        #   task_type = instance$task_type,
        #   n_features = instance$n_features,
        #   n_samples = instance$n_samples
        # )
      )

      if ("reference_proportion" %in% names(dots)) {
        result$max_reference_size <- method_params$max_reference_size
      }
      if ("conditioning_features" %in% names(dots) | method_name == "RFI") {
        result$conditioning_set <- method_params$conditioning_set
      }

      return(result)
    }
  )
}

# Define all algorithms using the factory
create_fi_algorithm("PFI", PFI)
create_fi_algorithm("CFI", CFI)
create_fi_algorithm("RFI", RFI)
create_fi_algorithm("LOCO", LOCO)
create_fi_algorithm("MarginalSAGE", MarginalSAGE)
create_fi_algorithm("ConditionalSAGE", ConditionalSAGE)
