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

res <- latest_reduced("importance", "xplainfi")
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
instance_key <- intersect(instance_key, names(res))

# reduce_importances() keeps only `importance` and `runtime` from each job's
# result, so the importance lane's `n_features` (a result column there, not a
# job parameter) is absent. The reduced table is long -- one row per feature per
# job -- so recover it by counting. The runtime lane has it as a problem
# parameter already, hence the guard.
if (!("n_features" %in% names(res))) {
	res[, n_features := .N, by = job.id]
}

# Cost in evaluated coalitions, the only axis on which the three estimators are
# directly comparable (see the estimator docs in xplainfi).
res[,
	n_evals := fcase(
		estimator == "permutation" , 1 + n_permutations * n_features ,
		estimator == "kernel"      , 2 + 2 * n_coalitions            ,
		estimator == "exact"       , 2^n_features
	)
]

# A readable label for the estimator configuration.
res[,
	arm := fcase(
		estimator == "kernel" , paste0("kernel-", kernel_variant) ,
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
		by = .(algorithm, arm, n_permutations, n_coalitions)
	]
	setorder(bias, algorithm, arm, mean_evals)

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
	# This is the check that exercises the new delta-method SE machinery rather
	# than just the point estimates.
	coverage <- paired[!is.na(conf_lower) & !is.na(conf_upper)][,
		.(
			n = .N,
			coverage = mean(conf_lower <= importance_exact & importance_exact <= conf_upper),
			mean_width = mean(conf_upper - conf_lower),
			mean_se = mean(se)
		),
		by = .(algorithm, arm, n_permutations, n_coalitions)
	]
	setorder(coverage, algorithm, arm, n_coalitions, n_permutations)

	cli::cli_h1("Check 2: Monte Carlo CI calibration (nominal 0.95)")
	cli::cli_alert_info(
		"Under-coverage means the reported SEs are too small. For MarginalSAGE the
		 SE conditions on the fixed reference subsample, so some under-coverage is
		 expected by construction -- compare arms against each other, not only
		 against 0.95."
	)
	print(coverage)
}

# ---------------------------------------------------------------------------
# Check 3: cross-implementation agreement
# ---------------------------------------------------------------------------
# xplainfi kernel_variant = "unbiased" is the only apples-to-apples pairing with
# the Python sage package, which implements exactly that variant. Point estimates
# should agree up to Monte Carlo error. Reported uncertainties are NOT comparable
# (sage includes observation-sampling noise; xplainfi conditions on the test set).
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
			cor = cor(importance_xplainfi, importance_sage),
			mean_diff = mean(importance_xplainfi - importance_sage),
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

out <- list(
	bias = if (exists("bias")) bias else data.table(),
	coverage = if (exists("coverage")) coverage else data.table(),
	cross = cross
)
saveRDS(out, here::here("results", "importance", "kernel-sage-validation.rds"))
cli::cli_alert_success("Wrote results/importance/kernel-sage-validation.rds")
