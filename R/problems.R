# Problem: Friedman1 regression task (fixed 10 features)
prob_friedman1 <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	learner_type = "rf",
	resampling_type = "holdout",
	...
) {
	task <- tgen("friedman1")$generate(n = n_samples)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		resampling_type = resampling_type,
		problem_name = "friedman1",
		conditioning_set = NULL,
		...
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
		resampling_type = resampling_type,
		conditioning_set = NULL,
		...
	)
}

# Problem: Bike Sharing (real-world regression task)
prob_bike_sharing <- function(
	data = NULL,
	job = NULL,
	# n_samples,
	learner_type = "rf",
	resampling_type = "holdout",
	convert_to_numeric = TRUE, # Convert factors to numeric for algorithm comparability
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

	# Optionally convert factors to numeric for fair algorithm comparison
	if (convert_to_numeric) {
		factor_cols <- names(xdat)[sapply(xdat, is.factor)]
		for (col in factor_cols) {
			# Convert factors to integers (1-based like in official SAGE example)
			xdat[[col]] <- as.integer(xdat[[col]])
		}
	}

	task = as_task_regr(xdat, target = "count", id = "bike_share")

	# Verify expected feature types based on conversion
	if (convert_to_numeric) {
		stopifnot(all(task$feature_types$type %in% c("numeric", "integer")))
		has_cats <- FALSE
	} else {
		stopifnot(setequal(unique(task$feature_types$type), c("numeric", "integer", "factor")))
		has_cats <- TRUE
	}

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		resampling_type = resampling_type,
		has_categoricals = has_cats,
		conditioning_set = NULL,
		convert_to_numeric = convert_to_numeric, # Pass through for metadata
		...
	)
}

# Problem: Correlated features (sim_dgp_correlated)
prob_correlated <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	learner_type = "rf",
	resampling_type = "holdout",
	correlation = 0.75,
	...
) {
	task <- sim_dgp_correlated(n = n_samples, r = correlation)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		resampling_type = resampling_type,
		correlation = correlation, # Additional metadata
		# Condition on x2 for correlated DGP
		conditioning_set = "x2",
		...
	)
}

# Problem: Ewald et al. (2024) DGP
prob_ewald <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	learner_type = "rf",
	resampling_type = "holdout",
	...
) {
	task <- sim_dgp_ewald(n = n_samples)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		resampling_type = resampling_type,
		conditioning_set = c("x1", "x2", "x3"),
		...
	)
}

# Problem: Interaction effects (sim_dgp_interactions)
prob_interactions <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	learner_type = "rf",
	resampling_type = "holdout",
	...
) {
	task <- sim_dgp_interactions(n = n_samples)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		resampling_type = resampling_type,
		conditioning_set = NULL,
		...
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
	...
) {
	task <- sim_dgp_independent(n = n_samples)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		resampling_type = resampling_type,
		conditioning_set = NULL,
		...
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
	...
) {
	task <- sim_dgp_confounded(n = n_samples, hidden = hidden)

	if ("x2" %in% task$feature_names) {
		task$set_col_roles("x2", remove_from = "feature")
	}

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		resampling_type = resampling_type,
		conditioning_set = "confounder",
		...
	)
}

# Problem: Mediation  (sim_dgp_mediated)
prob_mediated <- function(
	data = NULL,
	job = NULL,
	n_samples = 100,
	learner_type = "rf",
	resampling_type = "holdout",
	...
) {
	task <- sim_dgp_mediated(n = n_samples)

	create_problem_instance(
		task = task,
		job = job,
		learner_type = learner_type,
		resampling_type = resampling_type,
		conditioning_set = "mediator",
		...
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
		resampling_type = resampling_type,
		conditioning_set = NULL,
		...
	)
}
