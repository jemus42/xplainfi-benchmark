#! /usr/bin/env Rscript
# Shape checks for sage_algo_design(): row counts per arm, NA placement, and
# the per-implementation restriction of estimators/variants.
# Run: Rscript tests/test-sage-algo-design.R
suppressPackageStartupMessages(library(data.table))
source(here::here("R", "helpers.R"))

conf <- list(
	n_permutations = c(10, 50, 100),
	n_coalitions = c(32, 128, 512),
	kernel_variants = c("original", "unbiased"),
	sage_estimators = c("permutation", "kernel", "exact"),
	sage_early_stopping = FALSE,
	sage_n_samples = c(100),
	min_permutations = 20
)

# Full xplainfi axis: 3 permutation + 2 variants x 3 budgets + 1 exact = 10.
d <- sage_algo_design(conf)
stopifnot(nrow(d) == 10L)
stopifnot(identical(sort(unique(d$estimator)), c("exact", "kernel", "permutation")))

# The budget argument of a non-owning estimator must be NA, or batchtools would
# pass it to the constructor and the job would abort.
stopifnot(all(is.na(d[estimator != "permutation", n_permutations])))
stopifnot(all(is.na(d[estimator != "kernel", n_coalitions])))
stopifnot(all(is.na(d[estimator != "kernel", kernel_variant])))
stopifnot(all(!is.na(d[estimator == "permutation", n_permutations])))
stopifnot(all(!is.na(d[estimator == "kernel", n_coalitions])))

# Permutation-only convergence controls must not leak onto the other arms,
# where a non-default value warns.
stopifnot(all(is.na(d[estimator != "permutation", early_stopping])))
stopifnot(all(is.na(d[estimator != "permutation", min_permutations])))

# sage_n_samples applies to every estimator.
stopifnot(all(!is.na(d$sage_n_samples)))

# Column types must not depend on which estimators were requested. Before this
# was pinned down, n_coalitions came out double when the kernel arm was present
# and integer when it was not.
stopifnot(is.integer(d$n_permutations))
stopifnot(is.integer(d$n_coalitions))
stopifnot(is.character(d$estimator))
stopifnot(is.character(d$kernel_variant))

# Sampler cross-join multiplies rows and adds the column.
ds <- sage_algo_design(conf, sampler = c("gaussian", "knn"))
stopifnot(nrow(ds) == 20L)
stopifnot("sampler" %in% names(ds))
stopifnot(identical(sort(unique(ds$sampler)), c("gaussian", "knn")))

# Python sage: kernel + permutation, no variant choice, no exact arm.
dsage <- sage_algo_design(
	conf,
	estimators = c("permutation", "kernel"),
	kernel_variants = NA_character_
)
stopifnot(nrow(dsage) == 6L)
stopifnot(!("exact" %in% dsage$estimator))
stopifnot(all(is.na(dsage$kernel_variant)))
stopifnot(is.integer(dsage$n_permutations), is.integer(dsage$n_coalitions))

# fippy: permutation only.
dfippy <- sage_algo_design(conf, sampler = "simple", estimators = "permutation")
stopifnot(nrow(dfippy) == 3L)
stopifnot(all(dfippy$estimator == "permutation"))
stopifnot(is.integer(dfippy$n_permutations), is.integer(dfippy$n_coalitions))

# A typo must fail loudly, not return an empty design.
stopifnot(inherits(try(sage_algo_design(conf, estimators = "nope"), silent = TRUE), "try-error"))

# sage does not truncate to the requested budget, so the batch must be sized to
# it. The benchmark's grid on the cluster's 2-cpu allocation must come out exact.
# sage_batch_size() returns list(batch_size, n_jobs): n_jobs is reduced to the
# largest divisor of the budget, so the realised spend is exact by construction.
stopifnot(sage_batch_size(512L, n_jobs = 1L)$batch_size == 512L)
stopifnot(sage_batch_size(32L, n_jobs = 1L)$batch_size == 32L)
for (budget in c(10L, 50L, 100L)) {
	res <- sage_batch_size(budget, n_jobs = 2L)
	stopifnot(res$batch_size * res$n_jobs == budget)
}

# Exactness must hold even when the requested n_jobs is far larger than the
# budget or does not divide it -- this is the scenario that silently overspent
# off-cluster before the fix (n_threads() can be 48, sage_batch_size(10, 48)
# used to floor batch_size to 1 and spend 48 for a labelled budget of 10).
r10 <- sage_batch_size(10L, n_jobs = 48L)
stopifnot(r10$batch_size * r10$n_jobs == 10L)
r100 <- sage_batch_size(100L, n_jobs = 48L)
stopifnot(r100$batch_size * r100$n_jobs == 100L)

cat("OK: sage_algo_design\n")

# ---------------------------------------------------------------------------
# Guard: every design column must exist as a formal on the function that
# consumes it.
# ---------------------------------------------------------------------------
# batchtools passes every design column as a named argument (do.call with
# job$algo.pars, unfiltered), so a design column with no matching formal kills
# every job for that algorithm at runtime. Check the pairing here instead.
source(here::here("R", "algorithms.R"))
designs <- list(
	MarginalSAGE = sage_algo_design(conf),
	ConditionalSAGE = sage_algo_design(conf, sampler = "gaussian"),
	MarginalSAGE_sage = sage_algo_design(
		conf,
		estimators = c("permutation", "kernel"),
		kernel_variants = NA_character_
	),
	MarginalSAGE_fippy = sage_algo_design(conf, sampler = "simple", estimators = "permutation"),
	ConditionalSAGE_fippy = sage_algo_design(conf, sampler = "gaussian", estimators = "permutation")
)
for (nm in names(designs)) {
	extra <- setdiff(names(designs[[nm]]), names(formals(get(paste0("algo_", nm)))))
	if (length(extra) > 0) {
		stop(sprintf(
			"algo_%s lacks formals for design column(s): %s",
			nm,
			paste(extra, collapse = ", ")
		))
	}
}

cat("OK: every SAGE design column has a matching algo_* formal\n")
