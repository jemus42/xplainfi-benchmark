#! /usr/bin/env Rscript
# Exercises importance/analysis-kernel-sage.R end to end against a synthetic
# reduced table, without needing a real batchtools run.
#
# The analysis script has no unit-testable functions to call directly (it is a
# top-level script, per Step 2 of the task-7 brief), so this builds a fixture
# matching the reduce_importances() schema, saves it where latest_reduced()
# will find it, runs the analysis in a fresh Rscript subprocess (the least
# invasive way to exercise it end to end without restructuring the script),
# and inspects the .rds it writes. The fixture and that .rds are removed on
# exit, success or failure, so nothing is left in results/.
#
# Run from the repo root: Rscript tests/test-analysis-kernel-sage.R
suppressPackageStartupMessages(library(data.table))
source(here::here("setup-common.R")) # -> algo_provider(), save_reduced(), reduced_path()

fixture_version <- "test-fixture-kernel-sage"
xplainfi_fixture_path <- reduced_path("importance", "xplainfi", fixture_version)
reference_fixture_path <- reduced_path("importance", "reference", fixture_version)
out_path <- here::here("results", "importance", "kernel-sage-validation.rds")

# The whole test body lives in a function: on.exit() is a silent no-op at top
# level (nothing to attach it to), so cleanup would never run without this.
main <- function() {
	cleanup <- function() {
		if (fs::file_exists(xplainfi_fixture_path)) {
			fs::file_delete(xplainfi_fixture_path)
		}
		if (fs::file_exists(reference_fixture_path)) {
			fs::file_delete(reference_fixture_path)
		}
		if (fs::file_exists(out_path)) fs::file_delete(out_path)
	}
	on.exit(cleanup(), add = TRUE)
	stopifnot(
		!fs::file_exists(xplainfi_fixture_path),
		!fs::file_exists(reference_fixture_path),
		!fs::file_exists(out_path)
	)

	# -------------------------------------------------------------------------
	# Build the fixture: every arm the analysis pairs up, across 2 features and
	# 2 repls. n_features is deliberately NOT a column -- reduce_importances()
	# never produces it for the importance lane (it's a result column, not a
	# job parameter), so the analysis recovers it by counting feature-rows per
	# job.id. Omitting it here is what exercises that recovery path.
	#
	# job.id is numbered per provider block, restarting at 1 in each, mirroring
	# two real batchtools registries -- this is what makes xplainfi job.id 1
	# collide with reference job.id 1 once combine_reduced() stacks them, the
	# exact scenario the by = c("job.id", "provider", "xplainfi_version") fix
	# has to survive.
	# -------------------------------------------------------------------------
	arms <- list(
		list(
			algo = "MarginalSAGE",
			est = "permutation",
			nperm = 10L,
			ncoal = NA_integer_,
			kv = NA_character_
		),
		list(
			algo = "MarginalSAGE",
			est = "permutation",
			nperm = 50L,
			ncoal = NA_integer_,
			kv = NA_character_
		),
		list(algo = "MarginalSAGE", est = "kernel", nperm = NA_integer_, ncoal = 32L, kv = "original"),
		list(algo = "MarginalSAGE", est = "kernel", nperm = NA_integer_, ncoal = 128L, kv = "original"),
		list(algo = "MarginalSAGE", est = "kernel", nperm = NA_integer_, ncoal = 32L, kv = "unbiased"),
		list(algo = "MarginalSAGE", est = "kernel", nperm = NA_integer_, ncoal = 128L, kv = "unbiased"),
		list(
			algo = "MarginalSAGE",
			est = "exact",
			nperm = NA_integer_,
			ncoal = NA_integer_,
			kv = NA_character_
		),
		list(
			algo = "MarginalSAGE_sage",
			est = "kernel",
			nperm = NA_integer_,
			ncoal = 32L,
			kv = NA_character_
		),
		list(
			algo = "MarginalSAGE_sage",
			est = "kernel",
			nperm = NA_integer_,
			ncoal = 128L,
			kv = NA_character_
		)
	)

	features <- c("x1", "x2")
	repls <- 1:2
	truth <- c(x1 = 0.5, x2 = 0.3)

	set.seed(1)
	rows <- list()
	job_ids <- c(xplainfi = 0L, reference = 0L)
	for (arm in arms) {
		provider <- algo_provider(arm$algo)
		for (repl in repls) {
			job_ids[[provider]] <- job_ids[[provider]] + 1L
			job_id <- job_ids[[provider]]
			for (feat in features) {
				is_exact <- arm$est == "exact"
				budget <- if (arm$est == "permutation") arm$nperm else arm$ncoal
				se_val <- if (is_exact) NA_real_ else 0.05 / sqrt(budget)
				imp_val <- truth[[feat]] + if (is_exact) 0 else rnorm(1, sd = se_val)
				rows[[length(rows) + 1L]] <- data.table(
					job.id = job_id,
					feature = feat,
					importance = imp_val,
					se = se_val,
					conf_lower = if (is_exact) NA_real_ else imp_val - 1.96 * se_val,
					conf_upper = if (is_exact) NA_real_ else imp_val + 1.96 * se_val,
					algorithm = arm$algo,
					problem = "peak",
					learner_type = "linear",
					sampler = NA_character_,
					repl = repl,
					n_samples = 500,
					correlation = NA_real_,
					sage_n_samples = 100L,
					estimator = arm$est,
					kernel_variant = arm$kv,
					n_permutations = arm$nperm,
					n_coalitions = arm$ncoal,
					xplainfi_version = fixture_version,
					runtime = runif(1, 1, 5)
				)
			}
		}
	}
	fixture <- rbindlist(rows)
	fixture[, provider := algo_provider(algorithm)]
	stopifnot(!("n_features" %in% names(fixture)))

	# Mirror collect-results.R: one reduced table per provider present. This is
	# what exercises the fix for Defect 2 (analysis loading only the xplainfi
	# file and never seeing the reference arm needed for check 3).
	for (prov_name in unique(fixture$provider)) {
		invisible(save_reduced(
			fixture[provider == prov_name],
			"importance",
			provider = prov_name,
			version = fixture_version
		))
	}
	stopifnot(fs::file_exists(xplainfi_fixture_path), fs::file_exists(reference_fixture_path))

	# -------------------------------------------------------------------------
	# Run the real analysis script unmodified, in a subprocess, against the
	# fixture, then inspect what it wrote.
	# -------------------------------------------------------------------------
	proc <- system2(
		"Rscript",
		"importance/analysis-kernel-sage.R",
		stdout = TRUE,
		stderr = TRUE
	)
	status <- attr(proc, "status")
	if (!is.null(status) && status != 0) {
		stop("analysis-kernel-sage.R failed:\n", paste(proc, collapse = "\n"))
	}

	stopifnot(fs::file_exists(out_path))
	out <- readRDS(out_path)

	stopifnot(is.list(out), all(c("bias", "coverage", "cross") %in% names(out)))
	stopifnot(nrow(out$bias) > 0, nrow(out$coverage) > 0, nrow(out$cross) > 0)

	# The n_features recovery path feeds n_evals, which is only persisted to
	# disk in aggregate as bias$mean_evals -- an NA there is exactly the silent
	# failure the recovery path exists to prevent.
	stopifnot(all(is.finite(out$bias$mean_evals)))

	# Pin the recovery itself, not just its finiteness. The permutation arms'
	# n_evals = 1 + n_permutations * n_features is the one estimator whose cost
	# actually depends on n_features (kernel's 2 + 2 * n_coalitions does not), and
	# their job.ids (1-4 in the xplainfi block) collide with all four reference
	# job.ids by construction above. A regression to `by = job.id` alone recovers
	# n_features = 4 (2 real rows + 2 from the colliding reference job) instead of
	# the true 2, which is finite and plausible but wrong -- exactly what the
	# is.finite() check above cannot catch, and what this pins down instead.
	perm_bias <- out$bias[out$bias$arm == "permutation", ]
	stopifnot(nrow(perm_bias) > 0)
	expected_evals <- 1 + perm_bias$n_permutations * length(features)
	stopifnot(all(perm_bias$mean_evals == expected_evals))

	cat("OK: analysis-kernel-sage.R produces non-empty bias/coverage/cross tables\n")
	cat("    with n_features correctly recovered (mean_evals pinned exactly)\n")
}

main()
