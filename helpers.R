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
create_learner <- function(num.trees) {
  requireNamespace("mlr3learners", quietly = TRUE)
  lrn("regr.ranger", num.trees = num.trees, num.threads = 1)
}

# Helper function to create measure
create_measure <- function(task_type = "regr") {
  switch(task_type, "regr" = mlr3::msr("regr.mse"), "classif" = mlr3::msr("classif.ce"))
}
