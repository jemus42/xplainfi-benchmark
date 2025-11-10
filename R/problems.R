# Problem: Friedman1 regression task (fixed 10 features)
prob_friedman1 <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	learner_type = "rf",
	resampling_type = "holdout",
	n_trees = 500,
	...
) {
	task <- tgen("friedman1")$generate(n = n_samples)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		n_trees = n_trees,
		resampling_type = resampling_type,
		problem_name = "friedman1",
		conditioning_set = NULL
	)
}

# Problem: Peak regression task
prob_peak <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	n_features,
	learner_type = "rf",
	resampling_type = "holdout",
	n_trees = 500,
	...
) {
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

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		n_trees = n_trees,
		resampling_type = resampling_type,
		conditioning_set = NULL
	)
}

# Problem: Bike Sharing (real-world regression task)
prob_bike_sharing <- function(
	data = NULL,
	job = NULL,
	# n_samples,
	learner_type = "rf",
	resampling_type = "holdout",
	n_trees = 500,
	...
) {
	# Load bike sharing task (requires mlr3data)
	if (!requireNamespace("mlr3data", quietly = TRUE)) {
		stop("mlr3data package required for bike_sharing task")
	}

	xdat = mlr3misc::load_dataset("bike_sharing", package = "mlr3data")
	# Remove problematic features and convert logical to integer
	xdat[, date := NULL] # Remove character feature
	xdat[, holiday := as.integer(holiday)] # Convert logical to integer
	xdat[, working_day := as.integer(working_day)]

	task = as_task_regr(xdat, target = "count", id = "bike_share")

	stopifnot(setequal(unique(task$feature_types$type), c("numeric", "integer", "factor")))

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		n_trees = n_trees,
		resampling_type = resampling_type,
		has_categoricals = TRUE,
		conditioning_set = NULL
	)
}

# Problem: Correlated features (sim_dgp_correlated)
prob_correlated <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	learner_type = "rf",
	resampling_type = "holdout",
	n_trees = 500,
	correlation = 0.75,
	...
) {
	task <- sim_dgp_correlated(n = n_samples, r = correlation)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		n_trees = n_trees,
		resampling_type = resampling_type,
		correlation = correlation, # Additional metadata
		conditioning_set = "x2" # Condition on x2 for correlated DGP
	)
}

# Problem: Ewald et al. (2024) DGP
prob_ewald <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	learner_type = "rf",
	resampling_type = "holdout",
	n_trees = 500,
	...
) {
	task <- sim_dgp_ewald(n = n_samples)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		n_trees = n_trees,
		resampling_type = resampling_type,
		conditioning_set = c("x1", "x2", "x3")
	)
}

# Problem: Interaction effects (sim_dgp_interactions)
prob_interactions <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	learner_type = "rf",
	resampling_type = "holdout",
	n_trees = 500,
	...
) {
	task <- sim_dgp_interactions(n = n_samples)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		n_trees = n_trees,
		resampling_type = resampling_type,
		conditioning_set = NULL
	)
}

# Problem: Indepdendent (sim_dgp_independent)
prob_independent <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	hidden = FALSE,
	learner_type = "rf",
	resampling_type = "holdout",
	n_trees = 500,
	...
) {
	task <- sim_dgp_independent(n = n_samples)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		n_trees = n_trees,
		resampling_type = resampling_type,
		conditioning_set = NULL
	)
}

# Problem: Confounding (not hidden) (sim_dgp_confounded)
prob_confounded <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	hidden = FALSE,
	learner_type = "rf",
	resampling_type = "holdout",
	n_trees = 500,
	...
) {
	task <- sim_dgp_confounded(n = n_samples, hidden = hidden)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		n_trees = n_trees,
		resampling_type = resampling_type,
		conditioning_set = "confounder"
	)
}

# Problem: Mediation  (sim_dgp_mediated)
prob_mediated <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	hidden = FALSE,
	learner_type = "rf",
	resampling_type = "holdout",
	n_trees = 500,
	...
) {
	task <- sim_dgp_mediated(n = n_samples)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		n_trees = n_trees,
		resampling_type = resampling_type,
		conditioning_set = "mediator"
	)
}

# Simple problem for debugging
prob_debug <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	factors = TRUE,
	learner_type = "rf",
	resampling_type = "holdout",
	n_trees = 500,
	...
) {
	xdf <- data.table::data.table(x1 = rnorm(n_samples), x2 = rnorm(n_samples))
	if (factors) {
		xdf[,
			x3 := data.table::fcase(
				x2 > 1 , "a" ,
				x1 < 0 , "b" ,
				default = "c"
			)
		]
	}
	xdf[, y := x1 + rnorm(n_samples, sd = 0.1)]

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		n_trees = n_trees,
		resampling_type = resampling_type,
		conditioning_set = NULL
	)
}
