#! /usr/bin/env Rscript
# Asserts the installed xplainfi exposes the SAGE estimator axis from
# mlr-org/xplainfi#83 and enforces its documented constraints.
# Run: Rscript tests/check-kernel-sage-api.R
suppressPackageStartupMessages({
	library(mlr3)
	library(mlr3learners)
	library(xplainfi)
})
source(here::here("R", "helpers.R"))

task <- tgen("friedman1")$generate(n = 200)
task$select(task$feature_names[1:3])
learner <- lrn("regr.lm")

# Draws per variance block for this task, per the benchmark's mirror of
# xplainfi's internal check_interval. Checks 4/5 below pin it against the
# installed package.
block <- kernel_block_size(length(task$feature_names))

new_sage <- function(...) {
	MarginalSAGE$new(task = task, learner = learner, n_samples = 20, ...)
}

# 1. The estimator argument exists and all three values construct.
for (est in c("permutation", "kernel", "exact")) {
	stopifnot(inherits(new_sage(estimator = est), "MarginalSAGE"))
}

# n_permutations must be positively valid somewhere, or the exclusivity
# assertions below would also pass if the argument had simply been removed.
stopifnot(inherits(
	new_sage(estimator = "permutation", n_permutations = 10L),
	"MarginalSAGE"
))

# 2. Budget arguments are mutually exclusive -- a hard error, not a no-op.
#    This is what forces the rbind design instead of a CJ.
stopifnot(inherits(
	try(new_sage(estimator = "kernel", n_permutations = 10L), silent = TRUE),
	"try-error"
))
stopifnot(inherits(
	try(new_sage(estimator = "permutation", n_coalitions = 32L), silent = TRUE),
	"try-error"
))
stopifnot(inherits(
	try(new_sage(estimator = "exact", n_coalitions = 32L), silent = TRUE),
	"try-error"
))

# 3. kernel_variant is kernel-only and takes both documented values.
for (v in c("original", "unbiased")) {
	m <- new_sage(estimator = "kernel", n_coalitions = 2L * block, kernel_variant = v)
	stopifnot(inherits(m, "MarginalSAGE"))
	# Not just accepted -- actually stored. Every other assertion here would pass
	# if kernel_variant were silently ignored and the default used for both, which
	# would make the original-vs-unbiased comparison meaningless.
	stopifnot(identical(m$param_set$values$kernel_variant, v))
}
stopifnot(inherits(
	try(new_sage(estimator = "permutation", kernel_variant = "original"), silent = TRUE),
	"try-error"
))

# 4. montecarlo CIs are available for sampling estimators and rejected by exact.
#    TWO variance blocks is the smallest budget that produces SEs for
#    kernel_variant = "original" (batch means need two block estimates).
m <- new_sage(estimator = "kernel", n_coalitions = 2L * block)
m$compute()
imp <- m$importance(ci_method = "montecarlo")
stopifnot(all(c("feature", "importance", "se", "conf_lower", "conf_upper") %in% names(imp)))
stopifnot(all(is.finite(imp$se)))
stopifnot(all(is.finite(imp$conf_upper))) # two.sided default, not a one-sided Inf

e <- new_sage(estimator = "exact")
e$compute()
stopifnot(inherits(try(e$importance(ci_method = "montecarlo"), silent = TRUE), "try-error"))

# 5. Pin kernel_block_size() from below: ONE block must NOT yield SEs.
#    Together with check 4 this brackets the block size, so an upstream change to
#    check_interval in either direction fails here instead of silently mis-scaling
#    conf$n_coalition_blocks into errored jobs (or wasted draws).
one <- new_sage(estimator = "kernel", n_coalitions = block)
suppressWarnings(one$compute())
stopifnot(inherits(
	try(suppressWarnings(one$importance(ci_method = "montecarlo")), silent = TRUE),
	"try-error"
))

cat("OK: xplainfi", as.character(packageVersion("xplainfi")), "exposes the kernel SAGE API\n")
cat(
	"OK: kernel_block_size(",
	length(task$feature_names),
	") ==",
	block,
	"matches the installed package\n"
)
