# Configuration file for batchtools experiment
# Experiment settings
conf <- list(
	# General batchtools settings
	reg_path = fs::path(
		here::here("registries", "runtime"),
		paste0("xplainfi-", packageVersion("xplainfi"))
	),
	seed = 2025,
	repls = 50,
	# Samples to generate
	n_samples = c(100, 1000, 5000, 10000),
	# Only one task with variable number of features
	n_features = c(5, 10, 50),
	# Affects correlation task
	correlation = 0.5,
	# Affects PFI, CFI, RFI, and LOCO iterations
	n_repeats = c(1, 50, 100),
	# For SAGE permutations
	n_permutations = c(5, 10, 50),
	sage_early_stopping = c(TRUE, FALSE),
	# Size of sampled data used for Monte Carlo integration in SAGE methods
	sage_n_samples = c(10, 200, 500),
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
