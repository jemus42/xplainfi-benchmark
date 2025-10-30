# Algorithm definitions for batchtools experiment

# ============================================================================
# PFI - Permutation Feature Importance
# ============================================================================

algo_PFI <- function(data = NULL, job = NULL, instance, n_repeats = 1) {
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

addAlgorithm(name = "PFI", fun = algo_PFI)

# ============================================================================
# CFI - Conditional Feature Importance
# ============================================================================

algo_CFI <- function(
  data = NULL,
  job = NULL,
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

addAlgorithm(name = "CFI", fun = algo_CFI)

# ============================================================================
# RFI - Relative Feature Importance
# ============================================================================

algo_RFI <- function(
  data = NULL,
  job = NULL,
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

addAlgorithm(name = "RFI", fun = algo_RFI)

# ============================================================================
# LOCO - Leave-One-Covariate-Out
# ============================================================================

algo_LOCO <- function(data = NULL, job = NULL, instance, n_repeats = 1) {
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

addAlgorithm(name = "LOCO", fun = algo_LOCO)

# ============================================================================
# MarginalSAGE - Marginal SAGE
# ============================================================================

algo_MarginalSAGE <- function(
  data = NULL,
  job = NULL,
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

addAlgorithm(name = "MarginalSAGE", fun = algo_MarginalSAGE)

# ============================================================================
# ConditionalSAGE - Conditional SAGE
# ============================================================================

algo_ConditionalSAGE <- function(
  data = NULL,
  job = NULL,
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

addAlgorithm(name = "ConditionalSAGE", fun = algo_ConditionalSAGE)

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

algo_PFI_iml <- function(data = NULL, job = NULL, instance, n_repeats = 1) {
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

addAlgorithm(name = "PFI_iml", fun = algo_PFI_iml)

# ============================================================================
# PFI_vip - Reference implementation from vip package
# ============================================================================

algo_PFI_vip <- function(data = NULL, job = NULL, instance, n_repeats = 1) {
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

addAlgorithm(name = "PFI_vip", fun = algo_PFI_vip)

# ============================================================================
# PFI_fippy - Reference implementation from fippy package (Python)
# ============================================================================

algo_PFI_fippy <- function(data = NULL, job = NULL, instance, n_repeats = 1) {
  # Use first resampling iteration
  train_ids <- instance$resampling$train_set(1)
  test_ids <- instance$resampling$test_set(1)

  # Ensure Python packages (including pandas) are available before data conversion
  .ensure_python_packages()

  # Convert to sklearn format with pandas DataFrames (fippy samplers need .columns)
  sklearn_data <- task_to_sklearn(
    instance$task,
    train_ids,
    test_ids,
    as_pandas = TRUE
  )

  # Create and train sklearn learner
  sklearn_learner <- create_sklearn_learner(
    learner_type = instance$learner_type,
    task_type = instance$task_type,
    n_trees = 500,
    random_state = job$seed
  )

  sklearn_learner$fit(sklearn_data$X_train, sklearn_data$y_train)

  # Import fippy and create Explainer with SimpleSampler for marginal PFI
  fippy <- reticulate::import("fippy")
  sklearn_metrics <- reticulate::import("sklearn.metrics")

  sampler <- fippy$samplers$SimpleSampler(sklearn_data$X_train)
  loss_fn <- if (instance$task_type == "regr") {
    sklearn_metrics$mean_squared_error
  } else {
    sklearn_metrics$log_loss
  }

  explainer <- fippy$Explainer(
    predict = sklearn_learner$predict, # Pass the predict method, not the model
    X_train = sklearn_data$X_train,
    loss = loss_fn, # Pass function object, not string
    sampler = sampler
  )

  start_time <- Sys.time()

  # Compute PFI using fippy
  pfi_result <- explainer$pfi(
    X_eval = sklearn_data$X_test,
    y_eval = sklearn_data$y_test,
    nr_runs = as.integer(n_repeats)
  )

  end_time <- Sys.time()

  # Convert to standard format
  # fippy returns an explanation object with fi_means_stds() method
  fi_series <- pfi_result$fi_means_stds()
  importance_dt <- data.table::data.table(
    feature = instance$task$feature_names,
    importance = sapply(instance$task$feature_names, function(f) {
      as.numeric(fi_series[[f]])
    })
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

algo_CFI_fippy <- function(data = NULL, job = NULL, instance, n_repeats = 1) {
  # Use first resampling iteration
  train_ids <- instance$resampling$train_set(1)
  test_ids <- instance$resampling$test_set(1)

  # Ensure Python packages (including pandas) are available before data conversion
  .ensure_python_packages()

  # Convert to sklearn format with pandas DataFrames (fippy samplers need .columns)
  sklearn_data <- task_to_sklearn(
    instance$task,
    train_ids,
    test_ids,
    as_pandas = TRUE
  )

  # Create and train sklearn learner
  sklearn_learner <- create_sklearn_learner(
    learner_type = instance$learner_type,
    task_type = instance$task_type,
    n_trees = 500,
    random_state = job$seed
  )

  sklearn_learner$fit(sklearn_data$X_train, sklearn_data$y_train)

  # Import fippy and create Explainer with Gaussian sampler
  fippy <- reticulate::import("fippy")
  sklearn_metrics <- reticulate::import("sklearn.metrics")

  sampler <- fippy$samplers$GaussianSampler(sklearn_data$X_train)
  loss_fn <- if (instance$task_type == "regr") {
    sklearn_metrics$mean_squared_error
  } else {
    sklearn_metrics$log_loss
  }

  explainer <- fippy$Explainer(
    predict = sklearn_learner$predict, # Pass the predict method, not the model
    X_train = sklearn_data$X_train,
    loss = loss_fn, # Pass function object, not string
    sampler = sampler
  )

  start_time <- Sys.time()

  # Compute CFI using fippy
  cfi_result <- explainer$cfi(
    X_eval = sklearn_data$X_test,
    y_eval = sklearn_data$y_test,
    nr_runs = as.integer(n_repeats)
  )

  end_time <- Sys.time()

  # Convert to standard format
  # fippy returns an explanation object with fi_means_stds() method
  fi_series <- cfi_result$fi_means_stds()
  importance_dt <- data.table::data.table(
    feature = instance$task$feature_names,
    importance = sapply(instance$task$feature_names, function(f) {
      as.numeric(fi_series[[f]])
    })
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

# ============================================================================
# MarginalSAGE_fippy - Marginal SAGE from fippy package (Python)
# ============================================================================

algo_MarginalSAGE_fippy <- function(
  data = NULL,
  job = NULL,
  instance,
  n_permutations = 10,
  sage_n_samples = 10,
  detect_convergence = TRUE
) {
  # Use first resampling iteration
  train_ids <- instance$resampling$train_set(1)
  test_ids <- instance$resampling$test_set(1)

  # Ensure Python packages (including pandas) are available before data conversion
  .ensure_python_packages()

  # Convert to sklearn format with pandas DataFrames (fippy samplers need .columns)
  sklearn_data <- task_to_sklearn(
    instance$task,
    train_ids,
    test_ids,
    as_pandas = TRUE
  )

  # Create and train sklearn learner
  sklearn_learner <- create_sklearn_learner(
    learner_type = instance$learner_type,
    task_type = instance$task_type,
    n_trees = 500,
    random_state = job$seed
  )

  sklearn_learner$fit(sklearn_data$X_train, sklearn_data$y_train)

  # Import fippy and create Explainer with GaussianSampler
  fippy <- reticulate::import("fippy")
  sklearn_metrics <- reticulate::import("sklearn.metrics")

  sampler <- fippy$samplers$GaussianSampler(sklearn_data$X_train)
  loss_fn <- if (instance$task_type == "regr") {
    sklearn_metrics$mean_squared_error
  } else {
    sklearn_metrics$log_loss
  }

  explainer <- fippy$Explainer(
    predict = sklearn_learner$predict,
    X_train = sklearn_data$X_train,
    loss = loss_fn,
    sampler = sampler
  )

  start_time <- Sys.time()

  # Compute Marginal SAGE using fippy
  # nr_runs: number of permutation orderings (like n_permutations in xplainfi)
  # nr_resample_marginalize: number of samples for Monte Carlo integration (like n_samples in xplainfi)
  # Returns tuple (explanation, orderings) - we only need explanation
  sage_result <- explainer$msage(
    X_eval = sklearn_data$X_test,
    y_eval = sklearn_data$y_test,
    nr_runs = as.integer(n_permutations),
    nr_resample_marginalize = as.integer(n_samples),
    detect_convergence = detect_convergence
  )

  end_time <- Sys.time()

  # Extract explanation object (first element of tuple)
  explanation <- sage_result[[1]]
  fi_series <- explanation$fi_means_stds()
  importance_dt <- data.table::data.table(
    feature = instance$task$feature_names,
    importance = sapply(instance$task$feature_names, function(f) {
      as.numeric(fi_series[[f]])
    })
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

addAlgorithm(name = "MarginalSAGE_fippy", fun = algo_MarginalSAGE_fippy)

# ============================================================================
# ConditionalSAGE_fippy - Conditional SAGE from fippy package (Python)
# ============================================================================

algo_ConditionalSAGE_fippy <- function(
  data = NULL,
  job = NULL,
  instance,
  n_permutations = 10,
  sage_n_samples = 10,
  detect_convergence = TRUE
) {
  # Use first resampling iteration
  train_ids <- instance$resampling$train_set(1)
  test_ids <- instance$resampling$test_set(1)

  # Ensure Python packages (including pandas) are available before data conversion
  .ensure_python_packages()

  # Convert to sklearn format with pandas DataFrames (fippy samplers need .columns)
  sklearn_data <- task_to_sklearn(
    instance$task,
    train_ids,
    test_ids,
    as_pandas = TRUE
  )

  # Create and train sklearn learner
  sklearn_learner <- create_sklearn_learner(
    learner_type = instance$learner_type,
    task_type = instance$task_type,
    n_trees = 500,
    random_state = job$seed
  )

  sklearn_learner$fit(sklearn_data$X_train, sklearn_data$y_train)

  # Import fippy and create Explainer with GaussianSampler
  fippy <- reticulate::import("fippy")
  sklearn_metrics <- reticulate::import("sklearn.metrics")

  sampler <- fippy$samplers$GaussianSampler(sklearn_data$X_train)
  loss_fn <- if (instance$task_type == "regr") {
    sklearn_metrics$mean_squared_error
  } else {
    sklearn_metrics$log_loss
  }

  explainer <- fippy$Explainer(
    predict = sklearn_learner$predict,
    X_train = sklearn_data$X_train,
    loss = loss_fn,
    sampler = sampler
  )

  start_time <- Sys.time()

  # Compute Conditional SAGE using fippy
  # nr_runs: number of permutation orderings (like n_permutations in xplainfi)
  # nr_resample_marginalize: number of samples for Monte Carlo integration (like n_samples in xplainfi)
  # Returns tuple (explanation, orderings) - we only need explanation
  sage_result <- explainer$csage(
    X_eval = sklearn_data$X_test,
    y_eval = sklearn_data$y_test,
    nr_runs = as.integer(n_permutations),
    nr_resample_marginalize = as.integer(n_samples),
    detect_convergence = detect_convergence
  )

  end_time <- Sys.time()

  # Extract explanation object (first element of tuple)
  explanation <- sage_result[[1]]
  fi_series <- explanation$fi_means_stds()
  importance_dt <- data.table::data.table(
    feature = instance$task$feature_names,
    importance = sapply(instance$task$feature_names, function(f) {
      as.numeric(fi_series[[f]])
    })
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

addAlgorithm(name = "ConditionalSAGE_fippy", fun = algo_ConditionalSAGE_fippy)

# ============================================================================
# KernelSAGE - Official SAGE implementation with kernel estimator
# ============================================================================

algo_KernelSAGE <- function(
  data = NULL,
  job = NULL,
  instance,
  n_samples = NULL,
  sage_n_samples = 200
) {
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
    random_state = job$seed
  )

  sklearn_learner$fit(sklearn_data$X_train, sklearn_data$y_train)

  # Import sage and create MarginalImputer + KernelEstimator
  sage <- reticulate::import("sage")

  # Use training data as background data for marginalization
  # Limit to sage_n_samples to control computation
  n_background <- min(sage_n_samples, nrow(sklearn_data$X_train))
  background_data <- sklearn_data$X_train[1:n_background, , drop = FALSE]

  # Create imputer
  imputer <- sage$MarginalImputer(
    model = sklearn_learner,
    data = background_data
  )

  # Create KernelEstimator
  loss <- if (instance$task_type == "regr") "mse" else "cross entropy"
  estimator <- sage$KernelEstimator(
    imputer = imputer,
    loss = loss,
    random_state = job$seed
  )

  start_time <- Sys.time()

  # Compute SAGE values
  # n_samples parameter controls number of evaluations
  # Convert to numpy arrays explicitly to avoid shape attribute errors
  np <- reticulate::import("numpy", convert = FALSE)
  explanation <- estimator(
    X = np$array(sklearn_data$X_test),
    Y = np$array(sklearn_data$y_test),
    n_samples = n_samples, # NULL uses default convergence-based stopping
    detect_convergence = is.null(n_samples), # Only if n_samples not specified
    verbose = FALSE,
    bar = FALSE
  )

  end_time <- Sys.time()

  # Extract SAGE values from explanation object
  # explanation$values is a numpy array with shape (n_features,)
  importance_dt <- data.table::data.table(
    feature = instance$task$feature_names,
    importance = as.numeric(explanation$values)
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

addAlgorithm(name = "KernelSAGE", fun = algo_KernelSAGE)
