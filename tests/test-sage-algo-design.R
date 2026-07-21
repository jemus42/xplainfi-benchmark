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

# fippy: permutation only.
dfippy <- sage_algo_design(conf, sampler = "simple", estimators = "permutation")
stopifnot(nrow(dfippy) == 3L)
stopifnot(all(dfippy$estimator == "permutation"))

cat("OK: sage_algo_design\n")
