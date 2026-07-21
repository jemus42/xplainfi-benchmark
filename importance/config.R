# Configuration file for batchtools experiment
# Experiment settings

# Registry directory is namespaced by the xplainfi version actually installed,
# so results for different versions are retained side by side. Override with
# XPLAINFI_BENCH_VERSION to point at a historical registry (e.g. for collection).
xplainfi_version <- Sys.getenv(
	"XPLAINFI_BENCH_VERSION",
	unset = as.character(utils::packageVersion("xplainfi"))
)

# Which implementations to include: "xplainfi", "reference", or "all".
# Comma-separated. Use "xplainfi" to re-run only the package under test after a
# version bump without re-running the (frozen) reference implementations.
providers <- trimws(strsplit(
	Sys.getenv("XPLAINFI_BENCH_PROVIDERS", unset = "all"),
	","
)[[1]])

conf <- list(
	# General batchtools settings
	reg_path = fs::path(
		here::here("registries", "importance", paste0("xplainfi-", xplainfi_version))
	),
	providers = providers,
	seed = 2025,
	repls = 50,
	# Samples to generate
	n_samples = 5000,
	# Affects correlation task
	correlation = c(0.2, 0.5, 0.7, 0.9),
	# Affects PFI, CFI, and LOCO iterations
	n_repeats = 100,
	# SAGE permutations with convergence detection (across all implementations);
	# 100 is sufficient in practice.
	n_permutations = 100,
	min_permutations = 20,
	sage_early_stopping = TRUE,
	# Size of sampled data used for Monte Carlo integration in SAGE methods, 200 was usually sufficient
	# increases RAM usage a lot if set too high, and returns are diminishing somewhat quickly
	sage_n_samples = c(100),
	# Types of learners to use for each method, uses create_learner helper
	learner_types = c("linear", "rf", "mlp", "boosting"),
	# Conditional samplers for CFI and ConditionalSAGE
	samplers = c(
		"arf",
		"gaussian",
		"knn"
	)
)
