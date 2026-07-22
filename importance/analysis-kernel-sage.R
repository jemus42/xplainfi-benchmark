#! /usr/bin/env Rscript
# Validation of xplainfi's kernel SAGE estimator (mlr-org/xplainfi#83).
#
# Three checks, all joined on the semantic paired key -- NEVER job.id, which is
# registry-local. Instances are seed-synchronised across registries by the
# batchtools problem seed, so the same (problem, parameters, repl) is the same
# dataset regardless of which algorithms were present.
suppressPackageStartupMessages({
	library(data.table)
	library(ggplot2)
})
source(here::here("setup-common.R"))
source(here::here("importance", "config.R"))

# collect-results.R writes one reduced table per provider, so the reference arm
# (MarginalSAGE_sage) lives in a separate file from xplainfi's own methods.
# Check 3 compares the two, so both must be loaded.
res <- combine_reduced(
	latest_reduced("importance", "xplainfi"),
	latest_reduced("importance", "reference")
)
if (is.null(res) || nrow(res) == 0) {
	cli::cli_abort("No reduced importance results found. Run collect-results.R first.")
}
res <- as.data.table(res)

# The instance identity: everything that pins down which dataset was used.
# Deliberately excludes the estimator configuration, which is what we vary.
instance_key <- c(
	"problem",
	"algorithm",
	"learner_type",
	"sampler",
	"feature",
	"repl",
	"n_samples",
	"correlation",
	"sage_n_samples"
)

# intersect() below tolerates problem parameters that only some problems carry.
# These are not optional: dropping one silently turns the paired join into a
# many-to-many across that dimension instead of failing.
required_key <- c("problem", "algorithm", "feature", "repl")
missing_key <- setdiff(required_key, names(res))
if (length(missing_key) > 0) {
	cli::cli_abort(c(
		"Reduced table is missing join key column{?s} {.val {missing_key}}.",
		"i" = "Paired comparisons would silently become many-to-many. Re-run {.file collect-results.R}."
	))
}
instance_key <- intersect(instance_key, names(res))

# n_evals -- coalition evaluations, the cost axis comparable across estimators
# and implementations -- is now reported by every arm and carried through
# reduce_importances(), so it is read rather than re-derived. It used to be
# computed here from n_features, which the importance lane does not carry as a
# job parameter; recovering it by counting rows per job.id was wrong across
# combined registries, where job.id restarts at 1.
if (!("n_evals" %in% names(res))) {
	cli::cli_abort(c(
		"Reduced table has no {.field n_evals} column.",
		"i" = "It comes from the algo_* result. Re-run the jobs and {.file collect-results.R}."
	))
}

# A readable label for the estimator configuration. Early-stopped kernel rows are
# a separate arm: their budget is a ceiling, not a spend, so pooling them with the
# fixed-budget rows of the same variant would mix two different things.
res[,
	arm := fcase(
		estimator == "kernel" & !is.na(early_stopping) & early_stopping ,
		paste0("kernel-", kernel_variant, "-ES")                        ,
		estimator == "kernel"                                           ,
		paste0("kernel-", kernel_variant)                               ,
		default = estimator
	)
]

# ---------------------------------------------------------------------------
# Check 1: bias vs the exact estimator
# ---------------------------------------------------------------------------
# The exact arm has no coalition-sampling error, so on a given instance it is
# the ground truth the sampling arms should converge to.
truth <- res[estimator == "exact", c(instance_key, "importance"), with = FALSE]
setnames(truth, "importance", "importance_exact")

sampled <- res[estimator != "exact"]
paired <- merge(sampled, truth, by = instance_key)

if (nrow(paired) == 0) {
	cli::cli_warn("No paired exact-arm rows; skipping bias and coverage checks.")
} else {
	paired[, err := importance - importance_exact]

	bias <- paired[,
		.(
			n = .N,
			bias = mean(err),
			rmse = sqrt(mean(err^2)),
			mean_evals = mean(n_evals)
		),
		by = .(problem, algorithm, arm, n_permutations, n_coalitions)
	]
	setorder(bias, problem, algorithm, arm, mean_evals)

	cli::cli_h1("Check 1: bias vs the exact estimator")
	cli::cli_alert_info(
		"Expectation: RMSE falls with evaluated coalitions, and kernel-original
		 beats kernel-unbiased at equal cost (Covert & Lee, Section 4.1)."
	)
	print(bias)

	# ---------------------------------------------------------------------------
	# Check 2: Monte Carlo CI calibration
	# ---------------------------------------------------------------------------
	# Does the 95% montecarlo interval cover the exact value at the nominal rate?
	# This exercises the delta-method SE machinery. Coverage-of-exact is a valid
	# metric ONLY for arms whose point estimate is unbiased for the exact value:
	# kernel-unbiased, permutation, and the sage reference. The kernel-ORIGINAL
	# variant trades bias for a much lower variance (Covert & Lee Eq. 7), so its
	# tight interval sits around a biased centre and cannot cover the exact value
	# -- scoring it here would flunk a working estimator on the wrong axis. Its
	# bias is in Check 1; its (small, genuine) sampling SE is reported separately
	# just below. Pilot evidence: original SE ~2e-5 flat in budget with 7-17%
	# coverage, while unbiased shrinks 1/sqrt(budget) and covers at ~0.95.
	is_original <- grepl("^kernel-original", paired$arm)
	coverage <- paired[!is_original & !is.na(conf_lower) & !is.na(conf_upper)][,
		.(
			n = .N,
			coverage = mean(conf_lower <= importance_exact & importance_exact <= conf_upper),
			mean_width = mean(conf_upper - conf_lower),
			mean_se = mean(se)
		),
		by = .(problem, algorithm, arm, n_permutations, n_coalitions)
	]
	setorder(coverage, problem, algorithm, arm, n_coalitions, n_permutations)

	cli::cli_h1("Check 2: Monte Carlo CI calibration (nominal 0.95)")
	cli::cli_alert_info(
		"Unbiased / permutation / sage arms only. Under-coverage means the reported
		 SEs are too small. For MarginalSAGE the SE conditions on the fixed reference
		 subsample, so some under-coverage is expected by construction -- compare
		 arms against each other, not only against 0.95."
	)
	print(coverage)

	# kernel-original, judged on its own terms: bias vs sampling SE, no coverage.
	# The variant's whole point is a low variance around a slightly biased point,
	# so both numbers are reported and exact-coverage is deliberately omitted.
	orig <- paired[is_original]
	if (nrow(orig) > 0) {
		original_se <- orig[,
			.(
				n = .N,
				bias = mean(err),
				rmse = sqrt(mean(err^2)),
				mean_se = mean(se),
				se_zero_rate = mean(se == 0, na.rm = TRUE)
			),
			by = .(problem, algorithm, arm, n_coalitions)
		]
		setorder(original_se, problem, algorithm, arm, n_coalitions)
		cli::cli_h2("kernel-original: bias vs sampling SE (coverage-of-exact omitted)")
		cli::cli_alert_info(
			"mean_se near zero alongside a non-zero, budget-flat bias is the expected
			 signature: converged in variance to a biased point. Judge this arm by
			 bias (small, roughly flat in budget), never by exact-coverage."
		)
		print(original_se)
	}
}

# ---------------------------------------------------------------------------
# Check 4: does early stopping stop at a sensible point?
# ---------------------------------------------------------------------------
# Only kernel_variant = "original" carries an early-stopped arm: it is the
# shipped default and the estimator under test. "unbiased" exists here purely as
# the fixed-budget numerical bridge to the Python sage package, and cannot meet
# the default threshold at any tolerable budget in xplainfi's batch-averaged
# regime (~8k draws measured), so early stopping there would only exhaust the
# ceiling. See sage_algo_design() in R/helpers.R.
#
# Three things are asked of the stopped runs:
#   converged      how often the criterion was met before the ceiling
#   budget_used    what it cost, against the fixed-budget rows of the same variant
#   err_at_stop    error vs the exact arm where it stopped, versus the error the
#                  fixed budgets bought -- did it stop too early?
es <- if (nrow(paired) > 0 && "err" %in% names(paired)) {
	paired[grepl("-ES$", arm)]
} else {
	data.table()
}
if (nrow(es) > 0) {
	stopped <- es[,
		.(
			n = .N,
			converged_rate = mean(converged),
			median_used = median(budget_used),
			median_evals = median(n_evals),
			rmse_at_stop = sqrt(mean(err^2))
		),
		by = .(algorithm, arm, problem, sage_n_samples)
	]
	setorder(stopped, algorithm, arm, problem, sage_n_samples)

	cli::cli_h1("Check 4: early stopping")
	cli::cli_alert_info(
		"Compare rmse_at_stop against the fixed-budget rows in Check 1 at a similar
		 n_evals. Stopping is working if it reaches comparable error at lower cost;
		 it stopped too early if the error is materially worse than a fixed budget
		 of the same size."
	)
	print(stopped)

	# The interpretive crux. The SAGE standard errors quantify coalition-sampling
	# error ONLY -- for MarginalSAGE they condition on the fixed reference
	# subsample, so marginalization error is invisible to the stopping rule. If
	# the error at the stopping point falls with sage_n_samples while the reported
	# SE does not, then "converged" provably does not mean "accurate", and the
	# residual is the marginalization floor rather than an under-spent budget.
	if (uniqueN(es$sage_n_samples) > 1) {
		floor_check <- es[,
			.(n = .N, rmse_at_stop = sqrt(mean(err^2)), mean_se = mean(se)),
			by = .(algorithm, arm, sage_n_samples)
		]
		setorder(floor_check, algorithm, arm, sage_n_samples)
		cli::cli_h2("Error at the stopping point vs the marginalization budget")
		cli::cli_alert_info(
			"rmse_at_stop falling with {.field sage_n_samples} while {.field mean_se}
			 stays flat means the residual error is marginalization, which no extra
			 coalition budget would remove."
		)
		print(floor_check)
	}
} else {
	cli::cli_warn("No early-stopped rows found; skipping the early-stopping check.")
	stopped <- data.table()
}

# ---------------------------------------------------------------------------
# Check 3: cross-implementation agreement
# ---------------------------------------------------------------------------
# xplainfi kernel_variant = "unbiased" is the only apples-to-apples pairing with
# the Python sage package, which implements exactly that variant. Point estimates
# should agree up to Monte Carlo error. Reported uncertainties are NOT comparable
# (sage includes observation-sampling noise; xplainfi conditions on the test set).
#
# Only the bias signal (mean_diff) below is interpretable, and only with its
# Monte Carlo SE as a threshold for calling agreement. `sage` scores each
# coalition on a single resampled test row while xplainfi scores it on the
# whole test set, so both rmse and cor here are dominated by sage's
# observation-sampling noise, not by any xplainfi defect -- rmse is reported
# for reference only. cor is additionally near 1 practically by construction:
# it pools across features whose true importances differ by orders of
# magnitude, so it is not reported at all.
#
# The two implementations also draw their marginalization background
# differently, a real bias source that lands squarely in this check: `sage`
# uses the first `sage_n_samples` training rows (see the MarginalImputer
# construction in R/algorithms.R), whereas xplainfi draws a random n_samples
# subsample from the full task.
xpl <- res[algorithm == "MarginalSAGE" & arm == "kernel-unbiased"]
ref <- res[algorithm == "MarginalSAGE_sage" & estimator == "kernel"]

cross_key <- setdiff(instance_key, "algorithm")
if (nrow(xpl) > 0 && nrow(ref) > 0) {
	cmp <- merge(
		xpl[, c(cross_key, "n_coalitions", "importance"), with = FALSE],
		ref[, c(cross_key, "n_coalitions", "importance"), with = FALSE],
		by = c(cross_key, "n_coalitions"),
		suffixes = c("_xplainfi", "_sage")
	)
	cross <- cmp[,
		.(
			n = .N,
			mean_diff = mean(importance_xplainfi - importance_sage),
			se_mean_diff = sd(importance_xplainfi - importance_sage) / sqrt(.N),
			rmse = sqrt(mean((importance_xplainfi - importance_sage)^2))
		),
		by = .(problem, n_coalitions)
	]
	setorder(cross, problem, n_coalitions)

	cli::cli_h1("Check 3: xplainfi kernel-unbiased vs Python sage")
	print(cross)
} else {
	cli::cli_warn("No matched xplainfi/sage kernel rows; skipping cross-implementation check.")
	cross <- data.table()
}

# ---------------------------------------------------------------------------
# Check 5: cross-implementation agreement, permutation estimator
# ---------------------------------------------------------------------------
# xplainfi's permutation SAGE is the implementation under test. Sanity-check its
# point estimates against the exact truth AND against the independent reference
# implementations at a matched n_permutations budget: fippy (marginal AND
# conditional) and the Python sage package (marginal only). Paired on the DGP
# instance, NOT on `sampler`: for the marginal method `sampler` is an
# implementation label, not a data axis (xplainfi and sage report NA, fippy
# reports "simple"), so it is normalised to NA there and the instance drives the
# join. Conditional methods keep their real sampler, so an xplainfi sampler with
# no matching reference row simply does not pair, rather than pairing against the
# wrong sampler.
perm <- res[estimator == "permutation"]
exact_truth <- res[estimator == "exact"]
if (nrow(perm) > 0 && nrow(exact_truth) > 0) {
	norm_perm <- function(dt) {
		dt <- copy(dt)
		dt[, method := sub("_(fippy|sage)$", "", algorithm)]
		dt[,
			impl := fcase(
				grepl("_fippy$", algorithm) , "fippy" ,
				grepl("_sage$", algorithm)  , "sage"  ,
				default = "xplainfi"
			)
		]
		dt[, join_sampler := fifelse(grepl("^Marginal", method), NA_character_, as.character(sampler))]
		dt
	}
	perm <- norm_perm(perm)
	exact_truth <- norm_perm(exact_truth)
	pkey <- c(setdiff(cross_key, "sampler"), "join_sampler", "method")
	xpl_perm <- perm[impl == "xplainfi"]

	# One row per (reference, method, problem, n_permutations). Against fippy/sage
	# the budget is matched (both sample); against exact it is not (exact is
	# budget-free), so mean_diff there is xplainfi's bias vs truth at that budget.
	compare_ref <- function(ri) {
		r <- if (ri == "exact") exact_truth else perm[impl == ri]
		if (nrow(r) == 0) {
			return(data.table())
		}
		by_cols <- if (ri == "exact") pkey else c(pkey, "n_permutations")
		m <- merge(
			xpl_perm[, c(pkey, "n_permutations", "importance"), with = FALSE],
			r[, c(by_cols, "importance"), with = FALSE],
			by = by_cols,
			suffixes = c("_xpl", "_ref"),
			allow.cartesian = TRUE
		)
		if (nrow(m) == 0) {
			return(data.table())
		}
		m[,
			.(
				reference = ri,
				n = .N,
				mean_diff = mean(importance_xpl - importance_ref),
				se_mean_diff = sd(importance_xpl - importance_ref) / sqrt(.N),
				rmse = sqrt(mean((importance_xpl - importance_ref)^2))
			),
			by = .(method, problem, n_permutations)
		]
	}

	perm_cross <- rbindlist(
		lapply(c("exact", "fippy", "sage"), compare_ref),
		fill = TRUE
	)
	setorder(perm_cross, method, problem, reference, n_permutations)

	cli::cli_h1("Check 5: permutation cross-implementation")
	cli::cli_alert_info(
		"reference = exact: mean_diff is xplainfi's bias vs truth, should shrink as
		 n_permutations grows. reference = fippy/sage: mean_diff within a few
		 se_mean_diff of 0 means the two implementations agree at that budget. If
		 xplainfi tracks fippy but both drift from exact, the gap is the estimator,
		 not xplainfi."
	)
	print(perm_cross)
} else {
	cli::cli_warn(
		"No permutation or exact rows; skipping the permutation cross-implementation check."
	)
	perm_cross <- data.table()
}

out <- list(
	bias = if (exists("bias")) bias else data.table(),
	coverage = if (exists("coverage")) coverage else data.table(),
	original_se = if (exists("original_se")) original_se else data.table(),
	early_stopping = if (exists("stopped")) stopped else data.table(),
	cross = cross,
	perm_cross = if (exists("perm_cross")) perm_cross else data.table()
)
saveRDS(out, here::here("results", "importance", "kernel-sage-validation.rds"))
cli::cli_alert_success("Wrote results/importance/kernel-sage-validation.rds")
