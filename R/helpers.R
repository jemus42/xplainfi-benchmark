# Helper function to create resampling strategy
create_resampling <- function(
	type = "holdout",
	ratio = 2 / 3,
	folds = 3,
	repeats = 10
) {
	switch(
		type,
		"cv" = mlr3::rsmp("cv", folds = folds),
		"holdout" = mlr3::rsmp("holdout", ratio = ratio),
		"subsampling" = mlr3::rsmp("subsampling", ratio = ratio, repeats = repeats),
		"bootstrap" = mlr3::msr("bootstrap", ratio = 1, repeats = repeats)
	)
}

# Helper function to instantiate resampling reproducibly
# Ensures that:
# - Same task + same replication = same splits (for fair method comparison)
# - Same task + different replication = different splits (for independent runs)
instantiate_resampling <- function(resampling, task, replication = 1) {
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
	learner_type = c("rf", "linear", "featureless", "mlp", "boosting"),
	n_trees = 500,
	n_units = 5,
	task_type = c("regr", "classif"),
	task = NULL # Optional task to check for categorical features
) {
	requireNamespace("mlr3learners", quietly = TRUE)
	require("mlr3pipelines")
	learner_type <- match.arg(learner_type)
	task_type <- match.arg(task_type)

	base_learner <- switch(
		learner_type,
		"featureless" = {
			lrn(paste(task_type, "featureless", sep = "."))
		},
		"rf" = {
			lrn(paste(task_type, "ranger", sep = "."), num.trees = n_trees, num.threads = 1)
		},
		"linear" = {
			switch(task_type, regr = lrn("regr.lm"), classif = lrn("classif.log_reg"))
		},
		"mlp" = {
			require(mlr3torch)
			base_mlp <- lrn(
				paste(task_type, "mlp", sep = "."),
				# architecture parameters
				neurons = n_units,
				n_layers = 1,
				# training arguments
				batch_size = 32,
				epochs = 100,
				patience = 10,
				min_delta = 0.1,
				shuffle = TRUE,
				device = "cpu"
			)
			set_validate(base_mlp, validate = "test", ids = base_mlp$id)
			base_mlp
		},
		"boosting" = {
			base_xgb <- lrn(
				paste(task_type, "xgboost", sep = "."),
				nrounds = 1000,
				early_stopping_rounds = 50,
				eta = 0.1,
				booster = "gbtree",
				tree_method = "hist",
				nthread = 1
			)

			set_validate(base_xgb, validate = "test", ids = base_xgb$id)
			base_xgb
		}
	)

	# Check if encoding is needed based on task
	needs_encoding <- FALSE
	if (!is.null(task)) {
		# Check if task has categorical features (factor or character)
		feature_types <- task$feature_types$type
		has_categoricals <- any(feature_types %in% c("factor", "character"))

		# Determine which learners need encoding for categorical features
		# - RF (ranger) handles factors natively
		# - Linear models need encoding
		# - MLP needs encoding
		# - XGBoost needs encoding
		# - Featureless doesn't need features at all
		needs_encoding <- has_categoricals && learner_type %in% c("linear", "mlp", "boosting")
	}

	# Apply encoding pipeline if needed
	if (needs_encoding) {
		# Use treatment/one-hot encoding for categorical features
		# This handles factor and character columns automatically
		if (learner_type == "linear") {
			po_encode <- po("encode", method = "treatment")
		} else {
			po_encode <- po("encode", method = "one-hot")
		}
		learner <- po_encode %>>% base_learner
		learner <- as_learner(learner)

		# For XGBoost/MLP in pipeline, set validate on the graph learner
		if (learner_type %in% c("boosting", "mlp")) {
			# This enables early stopping for XGBoost when used in resample()
			# Note: validate="test" only works with resample(), not direct $train()
			set_validate(learner, validate = "test", ids = base_learner$id)
		}
	} else {
		learner <- base_learner

		# For XGBoost without pipeline, validate is already NULL (set above)
		# It will be set to "test" only when used with resample()
	}

	learner

	# Minimal prerpoc just to make things not break on accident
	# prepoc = po("fixfactors") %>>%
	#   po("imputesample", affect_columns = selector_type("factor")) %>>%
	#   po("removeconstants")

	# # Extra factor handling for linear model and mlp
	# if (learner_type %in% c("linear", "mlp")) {
	#   prepoc <- prepoc %>>%
	#     po("encode")
	# }

	# prepoc %>>%
	#   po("learner", base_learner) |>
	#   as_learner()
}

# Helper function to create measure
create_measure <- function(task_type = "regr") {
	importance = switch(
		task_type,
		"regr" = mlr3::msr("regr.mse"),
		"classif" = mlr3::msr("classif.ce")
	)
	eval = switch(
		task_type,
		"regr" = mlr3::msr("regr.rsq"),
		"classif" = mlr3::msr("classif.acc")
	)
	list(importance = importance, eval = eval)
}

# Helper function to create conditional sampler
create_sampler <- function(
	sampler = c("arf", "gaussian", "knn", "ctree"),
	task
) {
	sampler <- match.arg(sampler)

	switch(
		sampler,
		"arf" = ConditionalARFSampler$new(
			task,
			verbose = FALSE,
			finite_bounds = "local",
			min_node_size = 20,
			stepsize = 10000
		),
		"gaussian" = ConditionalGaussianSampler$new(task),
		"knn" = ConditionalKNNSampler$new(task, k = 5),
		"ctree" = ConditionalCtreeSampler$new(task)
	)
}

# Helper function to create complete problem instance
# Wraps common logic for all problems: creating learner, measure, resampling
create_problem_instance <- function(
	task,
	job = NULL,
	learner_type,
	n_trees = 500,
	resampling_type = "holdout",
	problem_name,
	has_categoricals = FALSE,
	...
) {
	task_type <- task$task_type

	# Create measure
	measures <- create_measure(task_type = task_type)
	# Create and instantiate resampling
	resampling <- create_resampling(type = resampling_type)
	instantiate_resampling(resampling, task, job$repl %||% 1)

	# Return instance with metadata - no learner created here!
	# Algorithms will create their own learners as needed
	list(
		task = task,
		measure = measures$importance,
		measure_eval = measures$eval,
		resampling = resampling,
		# Metadata
		n_features = length(task$feature_names),
		n_samples = task$nrow,
		task_type = task_type,
		learner_type = learner_type,
		resampling_type = resampling_type,
		has_categoricals = has_categoricals,
		... # Additional problem-specific metadata
	)
}
