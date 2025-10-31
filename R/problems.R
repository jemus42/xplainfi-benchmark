# Problem: Friedman1 regression task (fixed 10 features)
prob_friedman1 <- function(
	data,
	job,
	n_samples,
	learner_type = "ranger",
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
		problem_name = "friedman1"
	)
}

# Problem: Peak regression task
prob_peak <- function(
	data,
	job,
	n_samples,
	n_features,
	learner_type = "ranger",
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
		problem_name = "peak",
		n_features = n_features # Additional metadata
	)
}

# Problem: Bike Sharing (real-world regression task)
prob_bike_sharing <- function(
	data,
	job,
	# n_samples,
	learner_type = "ranger",
	resampling_type = "holdout",
	n_trees = 500,
	...
) {
	# Load bike sharing task (requires mlr3data)
	if (!requireNamespace("mlr3data", quietly = TRUE)) {
		stop("mlr3data package required for bike_sharing task")
	}

	xdat = mlr3data::bike_sharing
	xdat[, temperature := NULL]
	xdat[, date := NULL]
	xdat[, holiday := as.integer(holiday)]
	xdat[, working_day := as.integer(working_day)]

	task = as_task_regr(xdat, target = "count", id = "bike_share")

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		n_trees = n_trees,
		resampling_type = resampling_type,
		problem_name = "bike_sharing"
	)
}

# Problem: Correlated features (sim_dgp_correlated)
prob_correlated <- function(
	data,
	job,
	n_samples,
	learner_type = "ranger",
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
		problem_name = "correlated",
		correlation = correlation # Additional metadata
	)
}

# Problem: Ewald et al. (2024) DGP
prob_ewald <- function(
	data,
	job,
	n_samples,
	learner_type = "ranger",
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
		problem_name = "ewald"
	)
}

# Problem: Interaction effects (sim_dgp_interactions)
prob_interactions <- function(
	data,
	job,
	n_samples,
	learner_type = "ranger",
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
		problem_name = "interactions"
	)
}
