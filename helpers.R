# Helper function to create resampling strategy
create_resampling <- function(type = "cv", ratio = 2 / 3, folds = 3, repeats = 2) {
  switch(
    type,
    "cv" = mlr3::rsmp("cv", folds = folds),
    "holdout" = mlr3::rsmp("holdout", ratio = ratio),
    "repeated_cv" = mlr3::rsmp("repeated_cv", folds = folds, repeats = repeats)
  )
}

# Helper function to create learner
create_learner <- function(
  learner_type = c("ranger", "linear", "featureless"),
  n_trees = 100,
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
    }
  )

  # Minimal prerpoc just to make things not break on accident
  prepoc = po("fixfactors") %>>%
    po("imputesample", affect_columns = selector_type("factor")) %>>%
    po("removeconstants")

  # Extra factor handling for linear model
  if (learner_type == "linear") {
    prepoc <- prepoc %>>%
      po("encode")
  }

  prepoc %>>%
    po("learner", base_learner) |>
    as_learner()
}

# Helper function to create measure
create_measure <- function(task_type = "regr") {
  switch(
    task_type,
    "regr" = mlr3::msr("regr.mse"),
    "classif" = mlr3::msr("classif.ce")
  )
}
