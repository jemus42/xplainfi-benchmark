# Algorithm definitions for batchtools experiment

# ============================================================================
# PFI - Permutation Feature Importance
# ============================================================================

addAlgorithm(
  name = "PFI",
  fun = function(data, job, instance, n_repeats = 1) {
    browser()
    method <- PFI$new(
      task = instance$task,
      learner = instance$learner,
      measure = instance$measure,
      resampling = instance$resampling,
      n_repeats = n_repeats
    )

    start_time <- Sys.time()
    method$compute()
    end_time <- Sys.time()

    data.table::data.table(
      importance = list(method$importance()),
      runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
      n_features = instance$n_features,
      n_samples = instance$n_samples,
      task_type = instance$task_type,
      task_name = instance$name
    )
  }
)

# ============================================================================
# CFI - Conditional Feature Importance
# ============================================================================

addAlgorithm(
  name = "CFI",
  fun = function(
    data,
    job,
    instance,
    n_repeats = 1,
    sampler = "arf"
  ) {
    # Create sampler instance
    sampler_instance <- create_sampler(sampler = sampler, task = instance$task)

    method <- CFI$new(
      task = instance$task,
      learner = instance$learner,
      measure = instance$measure,
      resampling = instance$resampling,
      sampler = sampler_instance,
      n_repeats = n_repeats
    )

    start_time <- Sys.time()
    method$compute()
    end_time <- Sys.time()

    data.table::data.table(
      importance = list(method$importance()),
      runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
      n_features = instance$n_features,
      n_samples = instance$n_samples,
      task_type = instance$task_type,
      task_name = instance$name
    )
  }
)

# ============================================================================
# RFI - Relative Feature Importance
# ============================================================================

addAlgorithm(
  name = "RFI",
  fun = function(
    data,
    job,
    instance,
    n_repeats = 1,
    conditioning_set = NULL,
    sampler = "arf"
  ) {
    # Default conditioning set: first 2 feature
    if (is.null(conditioning_set)) {
      conditioning_set <- instance$task$feature_names[1:2]
    }

    # Create sampler instance
    sampler_instance <- create_sampler(sampler = sampler, task = instance$task)

    method <- RFI$new(
      task = instance$task,
      learner = instance$learner,
      measure = instance$measure,
      resampling = instance$resampling,
      conditioning_set = conditioning_set,
      sampler = sampler_instance,
      n_repeats = n_repeats
    )

    start_time <- Sys.time()
    method$compute()
    end_time <- Sys.time()

    data.table::data.table(
      importance = list(method$importance()),
      runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
      n_features = instance$n_features,
      n_samples = instance$n_samples,
      task_type = instance$task_type,
      task_name = instance$name,
      conditioning_set = list(conditioning_set)
    )
  }
)

# ============================================================================
# LOCO - Leave-One-Covariate-Out
# ============================================================================

addAlgorithm(
  name = "LOCO",
  fun = function(data, job, instance, n_repeats = 1) {
    method <- LOCO$new(
      task = instance$task,
      learner = instance$learner,
      measure = instance$measure,
      resampling = instance$resampling,
      n_repeats = n_repeats
    )

    start_time <- Sys.time()
    method$compute()
    end_time <- Sys.time()

    data.table::data.table(
      importance = list(method$importance()),
      runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
      n_features = instance$n_features,
      n_samples = instance$n_samples,
      task_type = instance$task_type,
      task_name = instance$name
    )
  }
)

# ============================================================================
# MarginalSAGE - Marginal SAGE
# ============================================================================

addAlgorithm(
  name = "MarginalSAGE",
  fun = function(
    data,
    job,
    instance,
    n_permutations = 10,
    sage_n_samples = 200
  ) {
    method <- MarginalSAGE$new(
      task = instance$task,
      learner = instance$learner,
      measure = instance$measure,
      resampling = instance$resampling,
      n_permutations = n_permutations,
      n_samples = sage_n_samples
    )

    start_time <- Sys.time()
    method$compute()
    end_time <- Sys.time()

    data.table::data.table(
      importance = list(method$importance()),
      runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
      n_features = instance$n_features,
      n_samples = instance$n_samples,
      task_type = instance$task_type,
      task_name = instance$name
    )
  }
)

# ============================================================================
# ConditionalSAGE - Conditional SAGE
# ============================================================================

addAlgorithm(
  name = "ConditionalSAGE",
  fun = function(
    data,
    job,
    instance,
    n_permutations = 10,
    sage_n_samples = 200,
    sampler = "arf"
  ) {
    # Create sampler instance
    sampler_instance <- create_sampler(sampler = sampler, task = instance$task)

    method <- ConditionalSAGE$new(
      task = instance$task,
      learner = instance$learner,
      measure = instance$measure,
      resampling = instance$resampling,
      sampler = sampler_instance,
      n_permutations = n_permutations,
      n_samples = sage_n_samples
    )

    start_time <- Sys.time()
    method$compute()
    end_time <- Sys.time()

    data.table::data.table(
      importance = list(method$importance()),
      runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
      n_features = instance$n_features,
      n_samples = instance$n_samples,
      task_type = instance$task_type,
      task_name = instance$name
    )
  }
)

# ============================================================================
# PFI_mlr3filters - Reference implementation from mlr3filters
# ============================================================================

# BB recommended to leave this out
# Side issue: Does not accept instantiated resamplings
# addAlgorithm(
#   name = "PFI_mlr3filters",
#   fun = function(data, job, instance, n_repeats = 1) {
#     require(mlr3filters)

#     filter_pfi <- flt(
#       "permutation",
#       learner = instance$learner,
#       measure = instance$measure,
#       resampling = instance$resampling,
#       nmc = n_repeats,
#       standardize = FALSE
#     )

#     start_time <- Sys.time()
#     filter_pfi$calculate(task = instance$task)
#     end_time <- Sys.time()

#     data.table::data.table(
#       importance = list(as.data.table(filter_pfi)),
#       runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
#       n_features = instance$n_features,
#       n_samples = instance$n_samples,
#       task_type = instance$task_type,
#       task_name = instance$name
#     )
#   }
# )

# ============================================================================
# PFI_iml - Reference implementation from iml package
# ============================================================================

addAlgorithm(
  name = "PFI_iml",
  fun = function(data, job, instance, n_repeats = 1) {
    require(iml)

    # iml requires a trained model, so we need to train first
    # Use the first resampling iteration (train/test split)
    train_ids <- instance$resampling$train_set(1)
    test_ids <- instance$resampling$test_set(1)

    # Clone learner to avoid modifying the instance
    learner_clone <- instance$learner$clone(deep = TRUE)

    # Train on training set
    learner_clone$train(instance$task, row_ids = train_ids)

    # Create iml Predictor object
    # iml expects a predict function that returns predictions
    predictor <- Predictor$new(
      model = learner_clone,
      data = instance$task$data(
        rows = test_ids,
        cols = instance$task$feature_names
      ),
      y = instance$task$data(
        rows = test_ids,
        cols = instance$task$target_names
      )[[1]],
      predict.function = function(model, newdata) {
        # Create temporary task for prediction
        temp_task <- instance$task$clone()
        temp_task$select(colnames(newdata))
        # Predict and return as vector
        preds <- model$predict_newdata(newdata, task = temp_task)
        if (instance$task_type == "classif") {
          # For classification, iml expects probabilities for the positive class
          # or just the response for binary
          preds$response
        } else {
          preds$response
        }
      }
    )

    start_time <- Sys.time()
    # Create FeatureImp object
    # iml computes importance on initialization
    imp <- FeatureImp$new(
      predictor = predictor,
      loss = ifelse(instance$task_type == "regr", "mse", "ce"),
      n.repetitions = n_repeats,
      compare = "difference" # Difference between permuted and original loss (matches xplainfi)
    )
    end_time <- Sys.time()

    imp_results <- imp$results

    # Convert iml results to standard format
    # iml returns: feature, importance (difference), importance.05, importance.95
    importance_dt <- data.table::data.table(
      feature = imp_results$feature,
      importance = imp_results$importance
    )

    data.table::data.table(
      importance = list(importance_dt),
      runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
      n_features = instance$n_features,
      n_samples = instance$n_samples,
      task_type = instance$task_type,
      task_name = instance$name
    )
  }
)

# ============================================================================
# PFI_vip - Reference implementation from vip package
# ============================================================================

addAlgorithm(
  name = "PFI_vip",
  fun = function(data, job, instance, n_repeats = 1) {
    require(vip)

    # vip requires a trained model, so we need to train first
    # Use the first resampling iteration (train/test split)
    train_ids <- instance$resampling$train_set(1)
    test_ids <- instance$resampling$test_set(1)

    # Clone learner to avoid modifying the instance
    learner_clone <- instance$learner$clone(deep = TRUE)

    # Train on training set
    learner_clone$train(instance$task, row_ids = train_ids)

    # Prepare data for vip
    test_data <- instance$task$data(rows = test_ids)
    target_name <- instance$task$target_names

    # Determine metric based on task type
    metric <- if (instance$task_type == "regr") "rmse" else "accuracy"

    # Create wrapper predict function for vip
    # vip expects a function(object, newdata) that returns predictions
    pred_wrapper <- function(object, newdata) {
      # Create temporary task for prediction
      temp_task <- instance$task$clone()
      temp_task$select(setdiff(colnames(newdata), target_name))

      # Predict
      if (is.function(object$predict_newdata)) {
        preds <- object$predict_newdata(newdata, task = temp_task)
      } else {
        stop("Learner does not have predict_newdata method")
      }

      if (instance$task_type == "classif") {
        # For classification, return class predictions
        preds$response
      } else {
        # For regression, return numeric predictions
        preds$response
      }
    }

    start_time <- Sys.time()

    # Compute permutation importance using vip
    # vip uses nsim for number of permutations
    imp_results <- vip::vi(
      object = learner_clone,
      method = "permute",
      train = test_data,
      target = target_name,
      metric = metric,
      nsim = n_repeats,
      pred_wrapper = pred_wrapper
    )

    end_time <- Sys.time()

    # Convert vip results to standard format
    # vip returns: Variable, Importance
    importance_dt <- data.table::as.data.table(imp_results)
    data.table::setnames(
      importance_dt,
      c("Variable", "Importance"),
      c("feature", "importance")
    )

    data.table::data.table(
      importance = list(importance_dt),
      runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
      n_features = instance$n_features,
      n_samples = instance$n_samples,
      task_type = instance$task_type,
      task_name = instance$name
    )
  }
)
# ============================================================================
# PFI_fippy - Reference implementation from fippy package (Python)
# ============================================================================

algo_PFI_fippy <- function(data, job, instance, n_repeats = 1) {
  # Use first resampling iteration
  train_ids <- instance$resampling$train_set(1)
  test_ids <- instance$resampling$test_set(1)

  # Convert to sklearn format
  sklearn_data <- task_to_sklearn(instance$task, train_ids, test_ids)

  # Create and train sklearn learner (ensures Python packages are available)
  sklearn_learner <- create_sklearn_learner(
    learner_type = instance$learner_type,
    task_type = instance$task_type,
    n_trees = 500,
    random_state = 712937L
  )

  sklearn_learner$fit(sklearn_data$X_train, sklearn_data$y_train)

  # Import fippy and create Explainer
  fippy <- reticulate::import("fippy")
  explainer <- fippy$Explainer$new(
    model = sklearn_learner,
    X_train = sklearn_data$X_train,
    loss = if (instance$task_type == "regr") "mse" else "cross_entropy"
  )

  start_time <- Sys.time()

  # Compute PFI using fippy
  pfi_result <- explainer$pfi(
    X_eval = sklearn_data$X_test,
    y_eval = sklearn_data$y_test,
    n_perm = as.integer(n_repeats)
  )

  end_time <- Sys.time()

  # Convert to standard format
  # fippy returns a dataframe with columns for each feature
  importance_dt <- data.table::data.table(
    feature = instance$task$feature_names,
    importance = as.numeric(pfi_result$score$mean())
  )

  data.table::data.table(
    importance = list(importance_dt),
    runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
    n_features = instance$n_features,
    n_samples = instance$n_samples,
    task_type = instance$task_type,
    task_name = instance$name,
    learner_type = instance$learner_type
  )
}

addAlgorithm(name = "PFI_fippy", fun = algo_PFI_fippy)

# ============================================================================
# CFI_fippy - Conditional Feature Importance from fippy (Python, Gaussian sampler)
# ============================================================================

algo_CFI_fippy <- function(data, job, instance, n_repeats = 1) {
  # Use first resampling iteration
  train_ids <- instance$resampling$train_set(1)
  test_ids <- instance$resampling$test_set(1)

  # Convert to sklearn format
  sklearn_data <- task_to_sklearn(instance$task, train_ids, test_ids)

  # Create and train sklearn learner (ensures Python packages are available)
  sklearn_learner <- create_sklearn_learner(
    learner_type = instance$learner_type,
    task_type = instance$task_type,
    n_trees = 500,
    random_state = 42L
  )

  sklearn_learner$fit(sklearn_data$X_train, sklearn_data$y_train)

  # Import fippy and create Explainer with Gaussian sampler
  fippy <- reticulate::import("fippy")
  sampler <- fippy$GaussianSampler$new(sklearn_data$X_train)

  explainer <- fippy$Explainer$new(
    model = sklearn_learner,
    X_train = sklearn_data$X_train,
    loss = if (instance$task_type == "regr") "mse" else "cross_entropy",
    sampler = sampler
  )

  start_time <- Sys.time()

  # Compute CFI using fippy
  cfi_result <- explainer$cfi(
    X_eval = sklearn_data$X_test,
    y_eval = sklearn_data$y_test,
    n_perm = as.integer(n_repeats)
  )

  end_time <- Sys.time()

  # Convert to standard format
  importance_dt <- data.table::data.table(
    feature = instance$task$feature_names,
    importance = as.numeric(cfi_result$score$mean())
  )

  data.table::data.table(
    importance = list(importance_dt),
    runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
    n_features = instance$n_features,
    n_samples = instance$n_samples,
    task_type = instance$task_type,
    task_name = instance$name,
    learner_type = instance$learner_type
  )
}

addAlgorithm(name = "CFI_fippy", fun = algo_CFI_fippy)
