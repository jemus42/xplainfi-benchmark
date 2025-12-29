# Configuration file for batchtools experiment
# Experiment settings
conf <- list(
	# General batchtools settings
	reg_path = fs::path(
		here::here("registries", "importance"),
		paste0("xplainfi-", packageVersion("xplainfi"))
	),
	seed = 2025,
	repls = 50,
	# Samples to generate
	n_samples = 5000,
	# Affects correlation task
	correlation = c(0.25, 0.75),
	# Affects PFI, CFI, RFI, and LOCO iterations
	n_repeats = 50,
	# For SAGE permutations: large n_permutations with convergence detection (across all implementations)
	n_permutations = c(100, 200),
	sage_early_stopping = TRUE,
	# Size of sampled data used for Monte Carlo integration in SAGE methods, 200 was usually sufficient
	# increases RAM usage a lot if set too high, and returns are diminishing somewhat quickly
	sage_n_samples = c(100, 200),
	# Types of learners to use for each method, uses create_learner helper
	learner_types = c("linear", "rf", "mlp", "boosting"),
	# Conditional samplers for CFI, RFI, and ConditionalSAGE
	samplers = c(
		"arf",
		"gaussian"
	)
)
