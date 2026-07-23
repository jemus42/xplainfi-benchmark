# Configuration file for batchtools experiment -- VALIDATION lane.
#
# This lane exists to *earn* the realistic settings the importance lane will use:
# it sweeps the budget factors (n_permutations, n_coalitions, sage_n_samples) AND
# carries the kernel early-stopping arm, so the analysis can see where each
# estimator converges, whether ES stops at a sensible point, and how the
# implementations agree. The importance lane then runs a single realistic budget
# with ES on; the runtime lane sweeps budgets with ES off. Keep those three
# distinct -- do not fold sweeps back into importance or ES into runtime.

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
		here::here("registries", "validation", paste0("xplainfi-", xplainfi_version))
	),
	providers = providers,
	seed = 2025,
	repls = 10,
	# Samples to generate
	n_samples = 5000,
	# Affects correlation task
	correlation = c(0.2, 0.5, 0.9),
	# Affects PFI and CFI
	n_repeats = 100,
	# SAGE permutation-estimator budget. Early stopping is off for this run so
	# the permutation arm spends a known budget and stays comparable, per
	# evaluated coalition, against the kernel and exact arms.
	n_permutations = c(10, 50, 100),
	# Floor on permutations before an early stop can be declared, so a lucky first
	# checkpoint cannot end the run. Inert while sage_early_stopping is FALSE, but
	# it must stay set: sage_algo_design() puts it on the design unconditionally,
	# and dropping it would hand the budget floor to algo_MarginalSAGE's own
	# default instead (see the note above sage_algo_design() in R/helpers.R).
	min_permutations = 20,
	sage_early_stopping = FALSE,
	# Convergence threshold for early stopping, matched across all implementations
	# (xplainfi se_threshold, sage/fippy thresh) so any ES comparison is fair. Set
	# to sage's default; xplainfi and fippy otherwise default to a stricter 0.01,
	# which needed ~6x more draws and looked like non-convergence. Not swept -- only
	# its consistency matters. See sage_algo_design() in R/helpers.R.
	se_threshold = 0.025,
	# SAGE kernel-estimator budget (paired coalition draws). Independent of
	# n_features, so evaluated-coalition cost differs across problems -- the
	# analysis reports cost explicitly rather than matching it in the design.
	n_coalitions = c(32, 128, 512),
	# Design-matrix ("A matrix") variant: "original" samples it alongside the
	# right-hand side (Covert & Lee Eq. 7), "unbiased" uses the exact closed form
	# (Eq. 9) and is what the Python sage package implements.
	kernel_variants = c("original", "unbiased"),
	# Which kernel variants additionally get an early-stopped row. "original" is
	# the shipped default and the estimator under test; "unbiased" exists only as
	# the numerical bridge to the Python sage package, which needs a matched fixed
	# budget on both sides. Measured: "unbiased" needs ~8k draws to meet the
	# default threshold in this batch-averaged regime, so early stopping there
	# only burns the ceiling. See sage_algo_design() in R/helpers.R.
	kernel_es_variants = "original",
	# Ceiling for early-stopped kernel rows: a bound, not a spend. "original"
	# typically converges in well under 100 draws.
	n_coalitions_ceiling = 2048,
	# "exact" enumerates all 2^n_features coalitions -- the ground truth this run
	# validates the sampling estimators against.
	sage_estimators = c("permutation", "kernel", "exact"),
	# Size of sampled data used for Monte Carlo integration in SAGE methods, 200 was usually sufficient
	# increases RAM usage a lot if set too high, and returns are diminishing somewhat quickly
	# Two values so the analysis can separate the two error sources. Both kernel
	# variants bottom out at the same accuracy regardless of coalition budget --
	# that floor is marginalization error set by this parameter, and the SAGE
	# standard errors do not capture it. If the error at the early-stopping point
	# falls with sage_n_samples, "converged" demonstrably does not mean "accurate".
	sage_n_samples = c(100, 400),
	# Types of learners to use for each method, uses create_learner helper
	learner_types = c("linear", "rf"), # "mlp", "boosting"),
	# Conditional samplers for CFI and ConditionalSAGE
	samplers = c(
		# "arf",
		"gaussian"
		# "knn"
	),
	# SAGE-only dev validation run (mlr-org/xplainfi#83): PFI/CFI/LOCO and their
	# reference implementations sit this out. Restore the full list to re-enable.
	methods = c(
		"MarginalSAGE",
		"ConditionalSAGE",
		"MarginalSAGE_sage",
		"MarginalSAGE_fippy",
		"ConditionalSAGE_fippy"
	)
)
