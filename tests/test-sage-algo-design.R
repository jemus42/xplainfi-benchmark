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
	min_permutations = 20,
	kernel_es_variants = "original",
	n_coalitions_ceiling = 2048
)

# Full xplainfi axis: 3 permutation + 2 variants x 3 fixed budgets
# + 1 early-stopped row for "original" + 1 exact = 11.
d <- sage_algo_design(conf)
stopifnot(nrow(d) == 11L)
stopifnot(identical(sort(unique(d$estimator)), c("exact", "kernel", "permutation")))

# The budget argument of a non-owning estimator must be NA, or batchtools would
# pass it to the constructor and the job would abort.
stopifnot(all(is.na(d[estimator != "permutation", n_permutations])))
stopifnot(all(is.na(d[estimator != "kernel", n_coalitions])))
stopifnot(all(is.na(d[estimator != "kernel", kernel_variant])))
stopifnot(all(!is.na(d[estimator == "permutation", n_permutations])))
stopifnot(all(!is.na(d[estimator == "kernel", n_coalitions])))

# early_stopping applies to the permutation AND kernel estimators, but warns for
# exact, so it must stay NA there. min_permutations remains permutation-only.
stopifnot(all(!is.na(d[estimator %in% c("permutation", "kernel"), early_stopping])))
stopifnot(all(is.na(d[estimator == "exact", early_stopping])))
stopifnot(all(is.na(d[estimator != "permutation", min_permutations])))

# The role split: "original" is the estimator under test and gets an
# early-stopped row on top of its fixed-budget curve; "unbiased" is the
# fixed-budget bridge to the Python sage package and must get none, since that
# comparison requires matched budgets on both sides.
stopifnot(nrow(d[kernel_variant == "original" & early_stopping]) == 1L)
stopifnot(nrow(d[kernel_variant == "unbiased" & early_stopping]) == 0L)
# The early-stopped row's budget is a ceiling, set above the whole fixed grid.
stopifnot(d[kernel_variant == "original" & early_stopping, n_coalitions] == 2048L)
stopifnot(all(
	d[(early_stopping), n_coalitions] > max(d[estimator == "kernel" & !early_stopping, n_coalitions])
))
# Requesting no ES variants drops the rows entirely (the reference arms).
stopifnot(nrow(sage_algo_design(conf, kernel_es_variants = character())) == 10L)

# A permutation budget at or below min_permutations can never reach the
# convergence check, so it spends in full and reports converged = FALSE while
# claiming to be an early-stopping arm. xplainfi checks the two arguments
# independently and never against each other, so this must abort here.
conf_es <- modifyList(conf, list(sage_early_stopping = TRUE))
stopifnot(inherits(try(sage_algo_design(conf_es), silent = TRUE), "try-error"))
# Only the offending budgets matter: raising the floor below the whole grid is fine.
stopifnot(is.data.table(
	sage_algo_design(modifyList(conf_es, list(min_permutations = 5)))
))
# ...and the check is scoped to the permutation estimator, which owns the floor.
stopifnot(is.data.table(sage_algo_design(conf_es, estimators = c("kernel", "exact"))))

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
stopifnot(nrow(ds) == 22L)
stopifnot("sampler" %in% names(ds))
stopifnot(identical(sort(unique(ds$sampler)), c("gaussian", "knn")))

# Python sage: kernel + permutation, no variant choice, no exact arm.
dsage <- sage_algo_design(
	conf,
	estimators = c("permutation", "kernel"),
	kernel_variants = NA_character_,
	kernel_es_variants = character()
)
stopifnot(nrow(dsage) == 6L)
stopifnot(!any(dsage$early_stopping))
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
		kernel_variants = NA_character_,
		kernel_es_variants = character()
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
