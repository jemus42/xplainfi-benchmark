#! /usr/bin/env Rscript
# End-to-end smoke test: every estimator runs through the algo functions on a
# tiny task, and montecarlo CIs appear exactly where they should.
# Run: Rscript tests/test-sage-algorithms.R
suppressPackageStartupMessages({
	library(mlr3)
	library(mlr3learners)
	library(xplainfi)
	library(data.table)
})
source(here::here("setup-common.R"))

# 3 features -> exact enumerates 8 coalitions, so this is fast.
# hidden = TRUE explicitly: prob_confounded's own default (hidden = FALSE)
# reveals the confounder as a 4th feature in the installed xplainfi version,
# which would break the "3 features" premise this test is built on.
inst <- prob_confounded(n_samples = 300, learner_type = "linear", hidden = TRUE)
stopifnot(inst$n_features == 3L)

run <- function(...) algo_MarginalSAGE(instance = inst, sage_n_samples = 20, ...)

cases <- list(
	permutation = list(estimator = "permutation", n_permutations = 5L, early_stopping = FALSE),
	kernel_orig = list(estimator = "kernel", n_coalitions = 16L, kernel_variant = "original"),
	kernel_unb = list(estimator = "kernel", n_coalitions = 16L, kernel_variant = "unbiased"),
	# Early stopping now applies to the kernel estimator. The budget becomes a
	# ceiling, so this must stop well short of it on a 3-feature task.
	kernel_es = list(
		estimator = "kernel",
		n_coalitions = 512L,
		kernel_variant = "original",
		early_stopping = TRUE
	),
	exact = list(estimator = "exact")
)

for (nm in names(cases)) {
	res <- do.call(run, cases[[nm]])
	imp <- res$importance[[1]]
	stopifnot(nrow(imp) == 3L)
	stopifnot(all(is.finite(imp$importance)))
	stopifnot(is.finite(res$runtime))

	# $budget replaced the deprecated $n_permutations_used. Reading the old field
	# warns, so its absence here also proves we are not still touching it.
	stopifnot(all(c("budget_requested", "budget_used", "n_evals", "converged") %in% names(res)))
	stopifnot(!("n_permutations_used" %in% names(res)))
	stopifnot(is.finite(res$n_evals), res$n_evals > 0)
	# Realised effort never exceeds what was asked for.
	stopifnot(res$budget_used <= res$budget_requested)

	if (identical(nm, "kernel_es")) {
		# The whole point of the arm: it stopped early rather than spending the
		# ceiling, and said so.
		stopifnot(res$converged)
		stopifnot(res$budget_used < res$budget_requested)
	}

	if (cases[[nm]]$estimator == "exact") {
		# Exact has no coalition-sampling error and rejects ci_method.
		stopifnot(!("conf_lower" %in% names(imp)))
	} else {
		stopifnot(all(c("se", "conf_lower", "conf_upper") %in% names(imp)))
		stopifnot(all(is.finite(imp$conf_lower)))
	}
	cat("  ", nm, "ok\n")
}

# NA in an inapplicable column must be inert, not passed to the constructor.
# This is exactly what batchtools does with an rbind(fill = TRUE) design row.
res <- run(
	estimator = "exact",
	n_permutations = NA_integer_,
	n_coalitions = NA_integer_,
	kernel_variant = NA_character_
)
stopifnot(nrow(res$importance[[1]]) == 3L)

# Conditional variant, one estimator, to confirm the sampler still threads through.
cres <- algo_ConditionalSAGE(
	instance = inst,
	estimator = "kernel",
	n_coalitions = 16L,
	kernel_variant = "original",
	sage_n_samples = 20,
	sampler = "gaussian"
)
stopifnot(nrow(cres$importance[[1]]) == 3L)

# Python sage reference arm: both estimators, explicit budgets, no convergence
# detection (so the budget is what we asked for, not what its detector picked).
for (est in c("kernel", "permutation")) {
	sres <- algo_MarginalSAGE_sage(
		instance = inst,
		estimator = est,
		n_coalitions = 16L,
		n_permutations = 5L,
		sage_n_samples = 20,
		early_stopping = FALSE
	)
	stopifnot(nrow(sres$importance[[1]]) == 3L)
	stopifnot(all(is.finite(sres$importance[[1]]$importance)))
	cat("   sage", est, "ok\n")
}

cat("OK: SAGE algorithm functions\n")
