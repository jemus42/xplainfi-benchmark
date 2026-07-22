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
	repls = 10,
	# Samples to generate
	n_samples = c(100, 250, 1000, 5000, 10000),
	# Only one task with variable number of features
	n_features = c(5, 10, 25),
	# Affects PFI, CFI, and LOCO iterations
	n_repeats = c(1, 50),
	# For SAGE permutations
	n_permutations = c(10, 50, 100),
	min_permutations = 20,
	sage_early_stopping = FALSE,
	n_coalitions = c(32, 128, 512),
	kernel_variants = c("original", "unbiased"),
	# See importance/config.R: early stopping is scoped to the shipped default
	# variant; "unbiased" is the fixed-budget bridge to the Python sage package.
	kernel_es_variants = "original",
	n_coalitions_ceiling = 2048,
	sage_estimators = c("permutation", "kernel", "exact"),
	# Size of sampled data used for Monte Carlo integration in SAGE methods.
	# Two values so the marginalization budget contributes visible variance to
	# the cost curve; the importance lane holds it fixed instead.
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
	),
	# SAGE-only dev validation run (mlr-org/xplainfi#83).
	methods = c(
		"MarginalSAGE",
		"ConditionalSAGE",
		"MarginalSAGE_sage",
		"MarginalSAGE_fippy",
		"ConditionalSAGE_fippy"
	)
)
