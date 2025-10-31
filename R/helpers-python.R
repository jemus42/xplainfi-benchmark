# Python/fippy integration helpers

# Declare Python requirements once
# This creates an ephemeral environment with uv
.ensure_python_packages <- function() {
	reticulate::py_require(
		packages = c(
			"pandas", # Required for fippy samplers (DataFrames with .columns)
			"scikit-learn",
			# Fork for .to_numpy() bug only relevant in RFI
			# "git+https://github.com/jemus42/fippy@fix-rfi-to-numpy-bug",
			"git+https://github.com/gcskoenig/fippy",
			"sage-importance" # Official SAGE implementation
		)
	)
}

# Helper function to convert mlr3 task to scikit-learn format
# Returns list with X_train, X_test, y_train, y_test
# Handles categorical features via one-hot encoding
# If as_pandas=TRUE, returns pandas DataFrames (needed for fippy samplers)
task_to_sklearn <- function(task, train_ids, test_ids, as_pandas = FALSE) {
	# Get training data
	train_data <- task$data(rows = train_ids)
	X_train <- train_data[, task$feature_names, with = FALSE]
	y_train <- train_data[[task$target_names]]

	# Get test data
	test_data <- task$data(rows = test_ids)
	X_test <- test_data[, task$feature_names, with = FALSE]
	y_test <- test_data[[task$target_names]]

	# Identify categorical features
	factor_cols <- names(X_train)[sapply(X_train, is.factor)]
	char_cols <- names(X_train)[sapply(X_train, is.character)]
	categorical_cols <- c(factor_cols, char_cols)

	# One-hot encode categorical features if present
	if (length(categorical_cols) > 0) {
		# Combine train and test for consistent encoding
		combined <- rbind(X_train, X_test)
		n_train <- nrow(X_train)

		# One-hot encode using mlr3pipelines
		po_encode <- mlr3pipelines::po("encode", method = "one-hot")
		encoded_task <- mlr3::as_task_regr(
			cbind(combined, dummy_target = 1),
			target = "dummy_target",
			id = "temp"
		)
		encoded_task <- po_encode$train(list(encoded_task))[[1L]]
		encoded_data <- encoded_task$data()[, -"dummy_target"]

		# Split back
		X_train <- encoded_data[1:n_train, ]
		X_test <- encoded_data[(n_train + 1):nrow(encoded_data), ]
	}

	if (as_pandas) {
		# Convert to pandas DataFrames/Series for fippy
		# Both X and y need pandas objects (DataFrames have .columns, Series have .to_numpy())
		pd <- reticulate::import("pandas", convert = FALSE)
		list(
			X_train = pd$DataFrame(as.matrix(X_train), columns = names(X_train)),
			X_test = pd$DataFrame(as.matrix(X_test), columns = names(X_test)),
			y_train = pd$Series(as.vector(y_train)),
			y_test = pd$Series(as.vector(y_test))
		)
	} else {
		# Convert to matrices/vectors for sage and sklearn
		list(
			X_train = as.matrix(X_train),
			X_test = as.matrix(X_test),
			y_train = as.vector(y_train),
			y_test = as.vector(y_test)
		)
	}
}

# Helper function to create scikit-learn learner
create_sklearn_learner <- function(
	learner_type,
	task_type,
	n_trees = 500,
	random_state = 42L
) {
	.ensure_python_packages()

	sklearn <- reticulate::import("sklearn")

	if (learner_type == "featureless") {
		if (task_type == "regr") {
			sklearn$dummy$DummyRegressor(strategy = "mean")
		} else {
			sklearn$dummy$DummyClassifier(
				strategy = "most_frequent",
				random_state = random_state
			)
		}
	} else if (learner_type == "linear") {
		if (task_type == "regr") {
			sklearn$linear_model$LinearRegression()
		} else {
			sklearn$linear_model$LogisticRegression(
				random_state = random_state,
				max_iter = 200L
			)
		}
	} else if (learner_type == "ranger") {
		if (task_type == "regr") {
			sklearn$ensemble$RandomForestRegressor(
				n_estimators = as.integer(n_trees),
				random_state = random_state,
				n_jobs = 1L
			)
		} else {
			sklearn$ensemble$RandomForestClassifier(
				n_estimators = as.integer(n_trees),
				random_state = random_state,
				n_jobs = 1L
			)
		}
	} else if (learner_type == "nnet") {
		if (task_type == "regr") {
			sklearn$neural_network$MLPRegressor(
				hidden_layer_sizes = reticulate::tuple(5L),
				max_iter = 200L,
				random_state = random_state
			)
		} else {
			sklearn$neural_network$MLPClassifier(
				hidden_layer_sizes = reticulate::tuple(5L),
				max_iter = 200L,
				random_state = random_state
			)
		}
	} else {
		stop("Unknown learner_type: ", learner_type)
	}
}
