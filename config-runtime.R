# Configuration file for batchtools experiment
# Experiment settings
conf <- list(
	# General batchtools settings
	reg_path = fs::path(
		here::here("registries", "runtime", "xplainfi-1.0.0")
	),
	seed = 2025,
	repls = 50,
	# Samples to generate
	n_samples = c(100, 250, 1000, 5000, 10000),
	# Only one task with variable number of features
	n_features = c(5, 10, 20, 50),
	# Affects correlation task, does not affect runtime
	correlation = 0.5,
	# Affects PFI, CFI, RFI, and LOCO iterations
	n_repeats = c(1, 50, 100),
	# For SAGE permutations
	n_permutations = c(10, 20, 50, 100),
	sage_early_stopping = FALSE,
	# Size of sampled data used for Monte Carlo integration in SAGE methods
	sage_n_samples = c(10, 50, 100, 200),
	# Types of learners to use for each method, uses create_learner helper
	learner_types = c("featureless", "linear"),
	# Conditional samplers for CFI, RFI, and ConditionalSAGE
	samplers = c(
		"arf",
		"gaussian",
		"knn",
		"ctree"
	)
)
