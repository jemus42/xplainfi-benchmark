#! /usr/bin/env Rscript
# Shape checks for sage_algo_design(): row counts per arm, NA placement, and
# the per-implementation restriction of estimators/variants.
# Run: Rscript tests/test-sage-algo-design.R
suppressPackageStartupMessages(library(data.table))
source(here::here("R", "helpers.R"))

conf <- list(
	n_permutations = c(10, 50, 100),
	# In production this is the union of the per-problem block grids, resolved by
	# setup-batchtools.R; three literals here keep the shape assertions readable.
	n_coalitions = c(32, 128, 512),
	kernel_variants = c("original", "unbiased"),
	sage_estimators = c("permutation", "kernel", "exact"),
	sage_early_stopping = FALSE,
	sage_n_samples = c(100),
	min_permutations = 20,
	se_threshold = 0.025,
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
# ...and that ordering is now enforced, not merely asserted here: the fixed grid
# is block-relative and resolved per problem, so a ceiling that lands inside it
# is no longer visible by inspecting the config.
stopifnot(inherits(
	try(sage_algo_design(modifyList(conf, list(n_coalitions_ceiling = 512))), silent = TRUE),
	"try-error"
))
stopifnot(is.data.table(
	sage_algo_design(modifyList(conf, list(n_coalitions_ceiling = 513)))
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

# se_threshold: matched everywhere EXCEPT the ES rows, which sweep it. Without a
# sweep configured they fall back to the matched value, so lanes that do not opt
# in are unaffected.
stopifnot(all(d$se_threshold == conf$se_threshold))

conf_sweep <- modifyList(conf, list(es_se_thresholds = c(0.025, 0.01, 0.0025)))
dsw <- sage_algo_design(conf_sweep)
# One ES row per threshold, and every non-ES row still on the matched value. A
# blanket se_threshold assignment would silently flatten the sweep back to 0.025.
stopifnot(identical(
	sort(dsw[(early_stopping), se_threshold]),
	sort(c(0.025, 0.01, 0.0025))
))
stopifnot(all(dsw[!early_stopping | is.na(early_stopping), se_threshold] == conf$se_threshold))
# The sweep multiplies ES rows only; the fixed-budget and exact arms are untouched.
stopifnot(nrow(dsw) == nrow(d) + 2L)
stopifnot(identical(
	dsw[estimator != "kernel" | !early_stopping, -"se_threshold"],
	d[estimator != "kernel" | !early_stopping, -"se_threshold"]
))

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
# Kernel coalition grid: block sizes and per-problem budgets.
# ---------------------------------------------------------------------------
# The floor is 16 draws, above which it is 4 * n_features. These are the four
# problems the validation lane registers.
stopifnot(identical(kernel_block_size(c(4L, 5L, 10L, 12L)), c(16L, 20L, 40L, 48L)))
stopifnot(identical(kernel_block_size(2L), 16L)) # floor, not 8

stopifnot(identical(kernel_budgets(4L, c(2, 6, 20)), c(32L, 96L, 320L)))
stopifnot(identical(kernel_budgets(12L, c(2, 6, 20)), c(96L, 288L, 960L)))

# Fewer than two blocks has no standard errors, so the job aborts rather than
# reporting a wider interval. Must fail here, where it costs nothing.
stopifnot(inherits(try(kernel_budgets(10L, c(1, 6)), silent = TRUE), "try-error"))
# Vector n_features would silently recycle into a wrong grid.
stopifnot(inherits(try(kernel_budgets(c(4L, 10L), 2), silent = TRUE), "try-error"))

# Every budget must clear its own problem's two-block floor -- the property the
# whole block-relative grid exists to guarantee.
for (m in c(4L, 5L, 10L, 12L)) {
	stopifnot(all(kernel_budgets(m, c(2, 6, 20)) >= 2L * kernel_block_size(m)))
}

cat("OK: kernel_block_size / kernel_budgets\n")

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
