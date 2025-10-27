# Helper function to create resampling strategy
create_resampling <- function(
  type = "holdout",
  ratio = 2 / 3,
  folds = 3
) {
  switch(
    type,
    "cv" = mlr3::rsmp("cv", folds = folds),
    "holdout" = mlr3::rsmp("holdout", ratio = ratio)
  )
}

# Helper function to instantiate resampling reproducibly
# Ensures that:
# - Same task + same replication = same splits (for fair method comparison)
# - Same task + different replication = different splits (for independent runs)
instantiate_resampling <- function(resampling, task, replication) {
  # Generate task-specific seed from hash using digest
  task_seed <- digest::digest2int(task$hash)

  # Combine with replication number for variation across replications
  combined_seed <- task_seed + replication

  withr::with_seed(combined_seed, {
    resampling$instantiate(task)
  })

  resampling
}

# Helper function to create learner
create_learner <- function(
  learner_type = c("ranger", "linear", "featureless", "nnet"),
  n_trees = 500,
  task_type = c("regr", "classif")
) {
  requireNamespace("mlr3learners", quietly = TRUE)
  require("mlr3pipelines")
  learner_type <- match.arg(learner_type)
  task_type <- match.arg(task_type)

  base_learner <- switch(
    learner_type,
    "featureless" = {
      learner_id <- paste0(task_type, ".featureless")
      lrn(learner_id)
    },
    "ranger" = {
      learner_id <- paste0(task_type, ".ranger")
      lrn(learner_id, num.trees = n_trees, num.threads = 1)
    },
    "linear" = {
      if (task_type == "regr") {
        lrn("regr.lm")
      } else {
        lrn("classif.log_reg")
      }
    },
    "nnet" = {
      learner_id <- paste0(task_type, ".nnet")
      lrn(learner_id, size = 5, maxit = 200, decay = 0.01, trace = FALSE)
    }
  )

  base_learner

  # Minimal prerpoc just to make things not break on accident
  # prepoc = po("fixfactors") %>>%
  #   po("imputesample", affect_columns = selector_type("factor")) %>>%
  #   po("removeconstants")

  # # Extra factor handling for linear model and nnet
  # if (learner_type %in% c("linear", "nnet")) {
  #   prepoc <- prepoc %>>%
  #     po("encode")
  # }

  # prepoc %>>%
  #   po("learner", base_learner) |>
  #   as_learner()
}

# Helper function to create measure
create_measure <- function(task_type = "regr") {
  switch(
    task_type,
    "regr" = mlr3::msr("regr.mse"),
    "classif" = mlr3::msr("classif.ce")
  )
}

# Helper function to create complete problem instance
# Wraps common logic for all problems: creating learner, measure, resampling
create_problem_instance <- function(
  task,
  job,
  learner_type,
  n_trees,
  resampling_type,
  problem_name,
  ...
) {
  task_type <- task$task_type

  # Create learner
  learner <- create_learner(
    learner_type = learner_type,
    n_trees = n_trees,
    task_type = task_type
  )

  # Create measure
  measure <- create_measure(task_type = task_type)

  # Create and instantiate resampling
  resampling <- create_resampling(type = resampling_type)
  instantiate_resampling(resampling, task, job$repl)

  # Return complete instance with metadata
  list(
    task = task,
    learner = learner,
    measure = measure,
    resampling = resampling,
    # Metadata
    n_features = length(task$feature_names),
    n_samples = task$nrow,
    name = problem_name,
    task_type = task_type,
    learner_type = learner_type,
    resampling_type = resampling_type,
    ... # Additional problem-specific metadata
  )
}
