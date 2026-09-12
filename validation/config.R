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
	correlation = c(0.5, 0.9),
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
	# (xplainfi se_threshold, sage/fippy thresh) so any ES comparison is fair. This
	# is now the shared default of all three; fippy alone still defaults to a
	# stricter 0.01, which needed ~6x more draws and looked like non-convergence.
	# Not swept -- only its consistency matters. See sage_algo_design() in R/helpers.R.
	se_threshold = 0.025,
	# Thresholds for the early-stopped kernel rows only -- the one place this is
	# swept instead of matched. At the shared 0.025, kernel-original stops at
	# exactly two variance blocks every time (measured: linear and rf, marginal and
	# conditional, 4 and 10 features), because two blocks is the first checkpoint
	# at which its batch-means SEs exist. So a single threshold reports the SE
	# floor, 2 * max(16, 4 * n_features), not where the estimator converges.
	#
	# The values must straddle the ratio the estimator ACTUALLY reaches at that
	# floor, or the sweep is inert: the achieved ratio is ~0.001-0.0026, an order of
	# magnitude inside the matched 0.025, so anything looser passes at the first
	# checkpoint and reports the floor again. 0.01 measured identical to 0.025 on
	# both problems tested and was dropped as dead weight. Measured curve on
	# friedman1/ConditionalSAGE/linear (block 40), which is what these values are
	# sized against -- draws, then reported max(se):
	#
	#   0.025  ->   80 (2 blocks)  0.0082     <- 1 df, unstable: 0.0082-0.0197
	#   0.0025 ->  200 (5 blocks)  0.0177        across RNG streams at fixed budget
	#   0.001  ->  720 (18)        0.0075     <- from ~5 blocks the SE tracks 1/sqrt(n)
	#   0.0005 -> 1640 (41)        0.0037
	#
	# Note the non-monotonicity at the top: 2 blocks can report a SMALLER se than 5
	# blocks, which is the 1-df variance estimate, not a real precision gain. Read
	# the 0.025 row as the floor, not as a convergence point.
	#
	# Granularity is still one block, so the curve is coarse (40-draw steps at 10
	# features). The tightest values may exhaust n_coalitions_ceiling at full cost
	# -- 4098 evals, ~2.1x the largest fixed budget on bike_sharing -- which
	# xplainfi warns about and `converged = FALSE` records. That is a result, not a
	# failure: it says the tolerance is unreachable at a tolerable budget.
	es_se_thresholds = c(0.025, 0.0025, 0.001, 0.0005),
	# SAGE kernel-estimator budget, in variance blocks rather than raw coalition
	# draws. kernel_variant = "original" estimates its SEs as batch means over
	# blocks of max(16, 4 * n_features) draws, so fewer than two blocks yields no
	# SEs at all and errors the job -- an absolute grid is below that floor on the
	# wide problems (18 such errors in the 2026-09-11 pretest at 32 draws) and far
	# above exact-enumeration cost on the narrow ones. setup-batchtools.R resolves
	# these to per-problem draw counts via kernel_budgets() and prunes the cross
	# terms. 2 is the floor, 20 is comfortably converged.
	n_coalition_blocks = c(2, 6, 20),
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
