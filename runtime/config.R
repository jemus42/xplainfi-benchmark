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
		here::here("registries", "runtime", paste0("xplainfi-", xplainfi_version))
	),
	providers = providers,
	seed = 2025,
	repls = 50,
	# Samples to generate
	n_samples = c(100, 250, 1000, 5000, 10000),
	# Only one task with variable number of features
	n_features = c(5, 10, 25),
	# Affects PFI, CFI, and LOCO iterations
	n_repeats = c(1, 50),
	# For SAGE permutations
	n_permutations = c(10, 50, 100),
	sage_early_stopping = FALSE,
	# Size of sampled data used for Monte Carlo integration in SAGE methods
	sage_n_samples = c(10, 50),
	# Types of learners to use for each method, uses create_learner helper
	learner_types = c(
	# "featureless", 
	"linear"
	),
	# Conditional samplers for CFI and ConditionalSAGE
	samplers = c(
		# "arf",
		"gaussian"
		# "knn",
		# "ctree"
	)
)
