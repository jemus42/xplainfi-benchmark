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
	# Matched convergence threshold (see importance/config.R). Inert in this lane
	# while early stopping is off, but kept so every lane defines it in one place.
	se_threshold = 0.025,
	# SAGE kernel-estimator budget, in variance blocks rather than raw coalition
	# draws (see validation/config.R for the reasoning). This lane sweeps
	# n_features, so setup-batchtools.R resolves the grid per dimension and prunes
	# the cross terms -- the cost curve is then read per dimension, where a block
	# is a constant number of draws, not across them.
	n_coalition_blocks = c(2, 6, 20),
	kernel_variants = c("original", "unbiased"),
	# No early-stopping arm in the runtime lane: a converging estimator has a
	# variable, budget-dependent runtime that cannot be attributed to a fixed
	# budget, which defeats the point of a runtime-vs-budget sweep. Early stopping
	# is studied in the validation lane and used in the importance lane; here every
	# arm runs a fixed budget so its cost is exactly attributable.
	kernel_es_variants = character(0),
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
