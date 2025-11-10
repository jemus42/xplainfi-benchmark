# Python/fippy integration helpers

# Python and package versions for reproducibility
# Using minimum versions (>=) to allow uv to resolve compatible versions
# This ensures compatibility across Python 3.11+ while maintaining reproducibility
PYTHON_PACKAGES <- c(
	"numpy>=1.26.0", # Minimum version compatible with Python 3.12+
	"pandas>=2.1.0", # Minimum version compatible with Python 3.12+
	"scikit-learn>=1.3.0", # Stable version with good Python 3.12+ support
	"xgboost>=2.0.0", # For boosting learner (matches R xgboost)
	"git+https://github.com/gcskoenig/fippy@a7a37aa5511f7074ead3289c89b1ae80036982cb",
	"sage-importance>=0.0.4"
)

# Declare Python requirements once
# This creates an ephemeral environment with uv
# uv handles Python version selection and dependency resolution automatically
.ensure_python_packages <- function() {
	if (!reticulate::py_available()) {
		reticulate::py_require(
			packages = PYTHON_PACKAGES
		)
	}
}

# Helper function to convert mlr3 task to scikit-learn format
# Returns list with X_train, X_test, y_train, y_test
# Does NOT encode categorical features - that is now handled by:
#   - Learner: via create_sklearn_learner(..., encode = TRUE)
#   - Sampler: via create_fippy_sampler() using SequentialSampler with RFSamplers
# For classification tasks, encodes target labels as integers for fippy compatibility
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

	# For classification, encode target labels as integers
	# This is needed for fippy to work properly (numeric predictions can be averaged)
	label_encoder <- NULL
	if (task$task_type == "classif") {
		.ensure_python_packages()
		sklearn_preprocessing <- reticulate::import("sklearn.preprocessing")
		label_encoder <- sklearn_preprocessing$LabelEncoder()

		# Convert factors to characters first, then encode
		y_train_chr <- as.character(y_train)
		y_test_chr <- as.character(y_test)

		y_train <- label_encoder$fit_transform(y_train_chr)
		y_test <- label_encoder$transform(y_test_chr)
	}

	if (as_pandas) {
		# Ensure Python packages (including pandas) are available before data conversion
		.ensure_python_packages()

		# Convert to pandas DataFrames/Series for fippy
		# Both X and y need pandas objects (DataFrames have .columns, Series have .to_numpy())
		# Note: Categorical features remain as-is (factor/character columns)
		pd <- reticulate::import("pandas", convert = FALSE)

		# Convert factors to strings for pandas compatibility
		X_train_converted <- data.table::copy(X_train)
		X_test_converted <- data.table::copy(X_test)
		factor_cols <- names(X_train_converted)[sapply(X_train_converted, is.factor)]
		if (length(factor_cols) > 0) {
			X_train_converted[, (factor_cols) := lapply(.SD, as.character), .SDcols = factor_cols]
			X_test_converted[, (factor_cols) := lapply(.SD, as.character), .SDcols = factor_cols]
		}

		list(
			X_train = pd$DataFrame(X_train_converted),
			X_test = pd$DataFrame(X_test_converted),
			y_train = pd$Series(as.vector(y_train)),
			y_test = pd$Series(as.vector(y_test))
		)
	} else {
		# Convert to matrices/vectors for sage and sklearn
		# Note: Matrices don't support categorical, so this assumes numeric data only
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
	encode = FALSE,
	n_trees = 500,
	random_state = 42L
) {
	# Ensure random_state is integer
	random_state <- as.integer(random_state)
	.ensure_python_packages()
	sklearn <- reticulate::import("sklearn")
	xgb <- reticulate::import("xgboost")
	ce <- reticulate::import("category_encoders")

	if (learner_type == "linear") {
		if (task_type == "regr") {
			learner <- sklearn$linear_model$LinearRegression()
		} else {
			learner <- sklearn$linear_model$LogisticRegression(
				random_state = random_state,
				max_iter = 200L
			)
		}
	} else if (learner_type == "rf") {
		if (task_type == "regr") {
			learner <- sklearn$ensemble$RandomForestRegressor(
				n_estimators = as.integer(n_trees),
				random_state = random_state,
				n_jobs = 1L
			)
		} else {
			learner <- sklearn$ensemble$RandomForestClassifier(
				n_estimators = as.integer(n_trees),
				random_state = random_state,
				n_jobs = 1L
			)
		}
	} else if (learner_type == "mlp") {
		if (task_type == "regr") {
			learner <- sklearn$neural_network$MLPRegressor(
				hidden_layer_sizes = reticulate::tuple(5L),
				max_iter = 500L,
				random_state = random_state
			)
		} else {
			learner <- sklearn$neural_network$MLPClassifier(
				hidden_layer_sizes = reticulate::tuple(5L),
				max_iter = 500L,
				random_state = random_state
			)
		}
	} else if (learner_type == "boosting") {
		# XGBoost with parameters matching R implementation
		# Note: early_stopping_rounds requires validation data in fit()
		if (task_type == "regr") {
			learner <- xgb$XGBRegressor(
				n_estimators = 1000L,
				learning_rate = 0.1,
				booster = "gbtree",
				tree_method = "hist",
				early_stopping_rounds = 50L,
				random_state = random_state,
				n_jobs = 1L
			)
		} else {
			learner <- xgb$XGBClassifier(
				n_estimators = 1000L,
				learning_rate = 0.1,
				booster = "gbtree",
				tree_method = "hist",
				early_stopping_rounds = 50L,
				random_state = random_state,
				n_jobs = 1L
			)
		}
	} else {
		stop("Unknown learner_type: ", learner_type)
	}
	if (encode) {
		# For simplicity with mixed types, use encoders from category_encoders
		# These handle both numeric and categorical seamlessly
		# cols=NULL means auto-detect categorical columns (object dtype)
		if (learner_type == "rf") {
			# RF: Can handle categoricals directly with OrdinalEncoder
			encoder <- ce$OrdinalEncoder(handle_unknown = "value")
		} else if (learner_type == "linear") {
			# Linear: Use target encoding or one-hot with drop_first
			encoder <- ce$OneHotEncoder(handle_unknown = "value", use_cat_names = TRUE, drop_invariant = TRUE)
		} else {
			# MLP and XGBoost: Need one-hot encoding
			encoder <- ce$OneHotEncoder(handle_unknown = "value", use_cat_names = TRUE)
		}

		# Create pipeline with encoder
		learner <- sklearn$pipeline$make_pipeline(encoder, learner)
	}
	learner
}

# Helper function to fit sklearn learner with proper XGBoost early stopping
fit_sklearn_learner <- function(learner, X_train, y_train, X_test, y_test) {
	# Check if it's an XGBoost model that needs validation data for early stopping
	learner_class <- class(learner)[1]
	is_xgboost <- grepl("XGB", learner_class)

	# Check if it's a pipeline containing XGBoost
	is_pipeline <- "steps" %in% names(learner)
	if (!is_xgboost && is_pipeline) {
		# Get the last step of the pipeline (the actual learner)
		last_step <- learner$steps[[length(learner$steps)]]
		is_xgboost <- grepl("XGB", class(last_step[[2]])[1])
	}

	if (is_xgboost && !is_pipeline) {
		# Direct XGBoost model: provide validation data for early stopping
		learner$fit(X_train, y_train, eval_set = list(list(X_test, y_test)), verbose = FALSE)
	} else if (is_xgboost && is_pipeline) {
		# XGBoost in pipeline: need to transform test data through encoder first
		# Then pass eval_set to XGBoost step

		# Transform test data through all steps except the last (the model)
		encoder_steps <- learner$steps[1:(length(learner$steps) - 1)]
		X_test_transformed <- X_test
		for (step in encoder_steps) {
			transformer <- step[[2]]
			# Fit and transform if not fitted yet
			if (!("transform" %in% names(transformer))) {
				transformer$fit(X_train)
			}
			X_test_transformed <- transformer$transform(X_test_transformed)
		}

		# Get the name of the XGBoost step (last step)
		last_step_name <- learner$steps[[length(learner$steps)]][[1]]

		# Create fit_params for the XGBoost step
		eval_set_param <- paste0(last_step_name, "__eval_set")
		verbose_param <- paste0(last_step_name, "__verbose")

		# Create kwargs list with transformed test data
		fit_kwargs <- list()
		fit_kwargs[[eval_set_param]] <- list(list(X_test_transformed, y_test))
		fit_kwargs[[verbose_param]] <- FALSE

		# Call fit with the parameters
		do.call(learner$fit, c(list(X_train, y_train), fit_kwargs))
	} else {
		# For other learners, use standard fit
		learner$fit(X_train, y_train)
	}

	invisible(learner)
}

# Helper function to create fippy sampler based on task features and sampler type
# sampler: "simple" (bootstrap resample), "gaussian" (parametric), or "rf" (RF-based)
# - SimpleSampler: Resamples from observed data (no assumptions, works with categoricals)
# - GaussianSampler: Assumes multivariate Gaussian (continuous features only)
# - SequentialSampler with RF: Semi-parametric, handles mixed data
create_fippy_sampler <- function(task, X_train_pandas, sampler = "gaussian") {
	.ensure_python_packages()
	fippy <- reticulate::import("fippy")

	# Validate sampler
	checkmate::assert_choice(sampler, choices = c("simple", "gaussian", "rf"))

	# SimpleSampler: bootstrap resampling, no distributional assumptions
	if (sampler == "simple") {
		cli::cli_inform("Using SimpleSampler (bootstrap resampling)")
		return(fippy$samplers$SimpleSampler(X_train_pandas))
	}

	# Get task data to check for categorical features (for gaussian/rf validation)
	task_data <- task$data()
	feature_names <- task$feature_names

	# Identify categorical features
	categorical_features <- character(0)
	for (feat in feature_names) {
		if (is.factor(task_data[[feat]]) || is.character(task_data[[feat]])) {
			categorical_features <- c(categorical_features, feat)
		}
	}

	# Validate sampler choice with categorical features
	if (length(categorical_features) > 0 && sampler == "gaussian") {
		cli::cli_abort(c(
			"x" = "GaussianSampler cannot be used with categorical features",
			"i" = "Task has {length(categorical_features)} categorical feature{?s}: {.val {categorical_features}}",
			"i" = "Use {.code sampler = 'rf'} or {.code sampler = 'simple'} for tasks with categorical features"
		))
	}

	if (sampler == "gaussian") {
		# Use GaussianSampler (only valid for continuous features)
		cli::cli_inform("Using GaussianSampler")
		fippy$samplers$GaussianSampler(X_train_pandas)
	} else {
		# Use SequentialSampler with RF-based samplers
		cli::cli_inform(
			"Using SequentialSampler with RF samplers{if (length(categorical_features) > 0) glue::glue(' ({length(categorical_features)} categorical feature{ifelse(length(categorical_features) > 1, \"s\", \"\")})')}"
		)

		# Create RF-based samplers
		cat_sampler <- fippy$samplers$UnivRFSampler(X_train_pandas, cat_inputs = categorical_features)
		cont_sampler <- fippy$samplers$ContUnivRFSampler(
			X_train_pandas,
			cat_inputs = categorical_features
		)

		# Combine into SequentialSampler
		fippy$samplers$SequentialSampler(
			X_train_pandas,
			categorical_fs = categorical_features,
			cat_sampler = cat_sampler,
			cont_sampler = cont_sampler
		)
	}
}
