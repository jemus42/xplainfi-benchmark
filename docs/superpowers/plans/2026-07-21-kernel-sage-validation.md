# Kernel SAGE Validation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add xplainfi's in-development SAGE `estimator` axis (permutation / kernel / exact, with `kernel_variant` original / unbiased) to both benchmark lanes, alongside a budget-matched Python `sage` reference arm, so the new Kernel SAGE implementation can be validated against an exact ground truth.

**Architecture:** The estimator axis enters as new *algorithm design columns* (`estimator`, `n_coalitions`, `kernel_variant`), not as new algorithms — this keeps `algorithm` stable in the paired join key against frozen reference results. Because the three estimators take mutually exclusive budget arguments, the design is an `rbind` of per-estimator sub-designs with `NA` in inapplicable columns, built by one shared helper, and the algorithm functions dispatch on `estimator` to assemble constructor arguments conditionally. A `conf$methods` knob parks PFI/CFI/LOCO for this SAGE-only run.

**Tech Stack:** R 4.6, `batchtools` experiment registries, `rv` for R dependency pinning, `uv` for the Python venv, `reticulate` for the `sage` / `fippy` reference implementations, `data.table` throughout.

Spec: `docs/superpowers/specs/2026-07-21-kernel-sage-validation-design.md`

## Global Constraints

- **Format all R with `air format <file>`** before committing. Config in `air.toml`: tabs, indent-width 2, line-width 100. Every source file ends with a newline.
- **`docs/` is gitignored** (pkgdown default). Files under it need `git add -f`.
- **Never set `reg$cluster.functions`** in any script — cluster functions come from `batchtools.conf.R` only.
- **Thread parity is mandatory.** Every implementation gets the same CPU budget via `n_threads()` (`R/helpers.R`). Any Python estimator exposing a thread argument must receive `as.integer(n_threads())`. Biasing thread counts makes the runtime comparison meaningless.
- **Never join results on `job.id`** — it is registry-local. The paired key is `c(problem, algorithm, learner_type, sampler, feature, repl)` plus problem parameters, now plus `estimator`, `kernel_variant`, `n_coalitions`, `n_permutations`.
- **Environment for every command in this plan:**
  ```sh
  export XPLAINFI_BENCH_VERSION=1.1.0.9000-kernelsage
  export XPLAINFI_BENCH_PROVIDERS=all
  ```
  These live in `.envrc` (gitignored, sourced by hand — direnv is not installed). `export` is required; a bare assignment is a shell variable that `Sys.getenv()` cannot see, and the failure is silent (wrong registry path).
- **Do not commit `importance/config.R` or `runtime/config.R` trimming that predates this work.** Both files already carry uncommitted edits (narrowed `correlation`, `learner_types`, `samplers`). Those are the user's and are assumed present; Task 5 adds to them.
- **`repls = 10` in both lanes.** This is a development sanity check, not an exhaustive study.
- xplainfi API facts this plan depends on (verified against `kernel-sage` @ `334b59ef`):
  - `estimator` is one of `"permutation"`, `"kernel"`, `"exact"`.
  - Budget arguments are mutually exclusive: passing `n_permutations` with `estimator = "kernel"` is a **hard error**, not a no-op.
  - `early_stopping` / `min_permutations` / `se_threshold` / `check_interval` are permutation-only; a non-default value for another estimator **warns**.
  - `kernel_variant` is `"original"` (default) or `"unbiased"`, and is **only valid** for `estimator = "kernel"`.
  - `estimator = "exact"` aborts when `n_features > max_features` (default `12L`).
  - `$importance(ci_method = "montecarlo")` returns columns `feature`, `importance`, `se`, `conf_lower`, `conf_upper` (no `statistic` / `p.value`). Defaults are `conf_level = 0.95`, `alternative = "two.sided"`. The **exact** estimator rejects this `ci_method`.

## File Structure

| File | Status | Responsibility |
| --- | --- | --- |
| `rproject.toml` | Modify | Pin xplainfi to the `kernel-sage` PR branch. |
| `rv.lock` | Modify (generated) | Records the resolved SHA that `provenance.rds` reads. |
| `R/helpers.R` | Modify | Add `sage_algo_design()` — the single source of the estimator axis for both lanes. |
| `R/algorithms.R` | Modify | `algo_MarginalSAGE`, `algo_ConditionalSAGE`, `algo_MarginalSAGE_sage` gain the estimator axis. |
| `importance/config.R` | Modify | New estimator knobs, `methods` filter, `repls = 10`. |
| `runtime/config.R` | Modify | Same, plus `min_permutations`. |
| `importance/setup-batchtools.R` | Modify | Apply `conf$methods`; use `sage_algo_design()`; drop the infeasible exact arm. |
| `runtime/setup-batchtools.R` | Modify | Same, plus scope the early-stopping pruner to the permutation estimator. |
| `importance/analysis-kernel-sage.R` | Create | The three validation checks: bias vs exact, MC-CI calibration, cross-implementation. |
| `tests/check-kernel-sage-api.R` | Create | Asserts the installed xplainfi exposes the PR's API and enforces its constraints. |
| `tests/test-sage-algo-design.R` | Create | Asserts the design builder's shape: row counts, NA placement, per-arm restriction. |
| `tests/test-sage-algorithms.R` | Create | End-to-end smoke: every estimator runs through the algo functions on a tiny task. |

`tests/` is new — this repo has no test framework and does not need one. Each file is a plain `Rscript` using `stopifnot()`, runnable standalone, and is the smallest thing that fails if the logic breaks.

---

### Task 1: Switch xplainfi to the PR branch and pin the API

Establishes that the installed package actually has the PR's API before any code is written against it. The check is written first and is expected to fail against the currently installed `main` build.

**Files:**
- Modify: `rproject.toml:26` (the xplainfi dependency line)
- Modify: `rv.lock` (regenerated by `rv sync`)
- Create: `tests/check-kernel-sage-api.R`

**Interfaces:**
- Consumes: nothing.
- Produces: an installed `xplainfi` whose `MarginalSAGE$new()` accepts `estimator`, `n_coalitions`, `kernel_variant`, and whose `$importance()` accepts `ci_method = "montecarlo"`. Every later task assumes this.

- [ ] **Step 1: Write the failing API check**

Create `tests/check-kernel-sage-api.R`:

```r
#! /usr/bin/env Rscript
# Asserts the installed xplainfi exposes the SAGE estimator axis from
# mlr-org/xplainfi#83 and enforces its documented constraints.
# Run: Rscript tests/check-kernel-sage-api.R
suppressPackageStartupMessages({
	library(mlr3)
	library(mlr3learners)
	library(xplainfi)
})

task <- tgen("friedman1")$generate(n = 200)
task$select(task$feature_names[1:3])
learner <- lrn("regr.lm")

new_sage <- function(...) {
	MarginalSAGE$new(task = task, learner = learner, n_samples = 20, ...)
}

# 1. The estimator argument exists and all three values construct.
for (est in c("permutation", "kernel", "exact")) {
	stopifnot(inherits(new_sage(estimator = est), "MarginalSAGE"))
}

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
	stopifnot(inherits(
		new_sage(estimator = "kernel", n_coalitions = 16L, kernel_variant = v),
		"MarginalSAGE"
	))
}
stopifnot(inherits(
	try(new_sage(estimator = "permutation", kernel_variant = "original"), silent = TRUE),
	"try-error"
))

# 4. montecarlo CIs are available for sampling estimators and rejected by exact.
m <- new_sage(estimator = "kernel", n_coalitions = 16L)
m$compute()
imp <- m$importance(ci_method = "montecarlo")
stopifnot(all(c("feature", "importance", "se", "conf_lower", "conf_upper") %in% names(imp)))
stopifnot(all(is.finite(imp$conf_upper))) # two.sided default, not a one-sided Inf

e <- new_sage(estimator = "exact")
e$compute()
stopifnot(inherits(try(e$importance(ci_method = "montecarlo"), silent = TRUE), "try-error"))

cat("OK: xplainfi", as.character(packageVersion("xplainfi")), "exposes the kernel SAGE API\n")
```

- [ ] **Step 2: Run it to verify it fails against the currently installed build**

```sh
Rscript tests/check-kernel-sage-api.R
```

Expected: FAIL. The installed `main` build has no `estimator` argument, so the first `new_sage(estimator = "permutation")` errors with `unused argument (estimator = est)`.

- [ ] **Step 3: Point rproject.toml at the PR branch**

In `rproject.toml`, change the xplainfi dependency (currently `branch = "main"`):

```toml
  { name = "xplainfi", git = "https://github.com/mlr-org/xplainfi", branch = "kernel-sage" },
```

Leave every other dependency untouched — the reference implementations must stay frozen.

- [ ] **Step 4: Install and confirm the resolved SHA**

```sh
make r-deps
Rscript -e 'cat(as.character(packageVersion("xplainfi")), "\n")'
Rscript -e 'source("R/provenance.R"); cat(lockfile_sha("xplainfi"), "\n")'
```

Expected: version `1.1.0.9000`, SHA starting `334b59ef` (the PR head at time of writing; a newer PR commit is fine, note it in the commit message).

- [ ] **Step 5: Verify the environment variables are actually exported**

```sh
Rscript -e 'cat(Sys.getenv("XPLAINFI_BENCH_VERSION"), "|", Sys.getenv("XPLAINFI_BENCH_PROVIDERS"), "\n")'
```

Expected: `1.1.0.9000-kernelsage | all`

If it prints `|` with empty values, `.envrc` is missing `export` on those lines. Fix before continuing — `config.R` would otherwise silently fall back to the un-namespaced registry path.

- [ ] **Step 6: Run the API check to verify it passes**

```sh
Rscript tests/check-kernel-sage-api.R
```

Expected: `OK: xplainfi 1.1.0.9000 exposes the kernel SAGE API`

- [ ] **Step 7: Commit**

```sh
air format tests/check-kernel-sage-api.R
git add rproject.toml rv.lock tests/check-kernel-sage-api.R
git commit -m "build: pin xplainfi to the kernel-sage PR branch

Adds a standalone API check asserting the estimator axis exists and that
the mutually exclusive budget arguments error as documented -- that
constraint is what forces an rbind design rather than a CJ downstream."
```

---

### Task 2: The shared estimator-axis design builder

One builder for both lanes so the axis cannot drift between them. It is parameterised by which estimators and variants each implementation supports, because the three arms genuinely differ: `fippy` has no kernel estimator at all, and Python `sage`'s kernel estimator has no variant choice (it *is* the unbiased one).

**Files:**
- Modify: `R/helpers.R` (append at end of file)
- Create: `tests/test-sage-algo-design.R`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `sage_algo_design(conf, sampler = NULL, estimators = conf$sage_estimators, kernel_variants = conf$kernel_variants)` returning a `data.table` with columns `estimator` (character), `n_permutations` (integer, `NA` off the permutation arm), `n_coalitions` (integer, `NA` off the kernel arm), `kernel_variant` (character, `NA` off the kernel arm), `sage_n_samples` (numeric), `early_stopping` (logical, `NA` off the permutation arm), `min_permutations` (numeric, `NA` off the permutation arm), and `sampler` (character) when `sampler` is non-`NULL`. Tasks 5 and 6 call it.

- [ ] **Step 1: Write the failing test**

Create `tests/test-sage-algo-design.R`:

```r
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
```

- [ ] **Step 2: Run it to verify it fails**

```sh
Rscript tests/test-sage-algo-design.R
```

Expected: FAIL with `could not find function "sage_algo_design"`.

- [ ] **Step 3: Implement the builder**

Append to `R/helpers.R`:

```r
# SAGE estimator axis -------------------------------------------------------

# Build the algorithm design for a SAGE implementation.
#
# The three estimators take mutually exclusive budget arguments (passing
# n_permutations with estimator = "kernel" is an error, not a no-op), so the
# estimator axis is an rbind of per-estimator sub-designs with NA in the
# inapplicable columns -- never a CJ over all of them.
#
# conf             the lane's conf list
# sampler          character vector to cross-join, or NULL for marginal methods
# estimators       which estimators this implementation supports. fippy has only
#                  the permutation estimator; Python sage has kernel+permutation.
# kernel_variants  which design-matrix variants it supports. Pass NA_character_
#                  for implementations with no variant choice (Python sage's
#                  kernel estimator is always the unbiased one).
sage_algo_design <- function(
	conf,
	sampler = NULL,
	estimators = conf$sage_estimators,
	kernel_variants = conf$kernel_variants
) {
	parts <- list()

	if ("permutation" %in% estimators) {
		parts$permutation <- data.table::CJ(
			estimator = "permutation",
			n_permutations = conf$n_permutations,
			n_coalitions = NA_integer_,
			kernel_variant = NA_character_,
			sage_n_samples = conf$sage_n_samples,
			early_stopping = conf$sage_early_stopping,
			min_permutations = conf$min_permutations
		)
	}

	if ("kernel" %in% estimators) {
		parts$kernel <- data.table::CJ(
			estimator = "kernel",
			n_permutations = NA_integer_,
			n_coalitions = conf$n_coalitions,
			kernel_variant = kernel_variants,
			sage_n_samples = conf$sage_n_samples
		)
	}

	if ("exact" %in% estimators) {
		parts$exact <- data.table::CJ(
			estimator = "exact",
			n_permutations = NA_integer_,
			n_coalitions = NA_integer_,
			kernel_variant = NA_character_,
			sage_n_samples = conf$sage_n_samples
		)
	}

	d <- data.table::rbindlist(parts, fill = TRUE)

	if (!is.null(sampler)) {
		# Cross join. data.table's merge has no by = NULL, base merge does.
		d <- data.table::as.data.table(
			merge(as.data.frame(d), data.frame(sampler = sampler, stringsAsFactors = FALSE))
		)
	}

	d[]
}
```

- [ ] **Step 4: Run the test to verify it passes**

```sh
Rscript tests/test-sage-algo-design.R
```

Expected: `OK: sage_algo_design`

- [ ] **Step 5: Commit**

```sh
air format R/helpers.R tests/test-sage-algo-design.R
git add R/helpers.R tests/test-sage-algo-design.R
git commit -m "feat: add sage_algo_design() estimator-axis builder

Parameterised by supported estimators and variants per implementation:
fippy has no kernel estimator, and Python sage's kernel estimator has no
variant choice. A uniform builder would emit kernel_variant='original'
jobs for implementations that cannot express it -- jobs that would run
and return plausible but mislabelled numbers."
```

---

### Task 3: xplainfi SAGE algorithm functions gain the estimator axis

**Files:**
- Modify: `R/algorithms.R:127-171` (`algo_MarginalSAGE`)
- Modify: `R/algorithms.R:178-227` (`algo_ConditionalSAGE`)
- Create: `tests/test-sage-algorithms.R`

**Interfaces:**
- Consumes: the installed API from Task 1.
- Produces: `algo_MarginalSAGE(data, job, instance, estimator, n_permutations, n_coalitions, kernel_variant, sage_n_samples, batch_size, early_stopping, min_permutations)` and `algo_ConditionalSAGE(...)` with the same signature plus `sampler`. Both return the existing result `data.table`, whose `importance` element now carries `se` / `conf_lower` / `conf_upper` for the sampling estimators. Task 6 registers these against `sage_algo_design()` output.

- [ ] **Step 1: Write the failing smoke test**

Create `tests/test-sage-algorithms.R`:

```r
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
inst <- prob_confounded(n_samples = 300, learner_type = "linear")
stopifnot(inst$n_features == 3L)

run <- function(...) algo_MarginalSAGE(instance = inst, sage_n_samples = 20, ...)

cases <- list(
	permutation = list(estimator = "permutation", n_permutations = 5L, early_stopping = FALSE),
	kernel_orig = list(estimator = "kernel", n_coalitions = 16L, kernel_variant = "original"),
	kernel_unb = list(estimator = "kernel", n_coalitions = 16L, kernel_variant = "unbiased"),
	exact = list(estimator = "exact")
)

for (nm in names(cases)) {
	res <- do.call(run, cases[[nm]])
	imp <- res$importance[[1]]
	stopifnot(nrow(imp) == 3L)
	stopifnot(all(is.finite(imp$importance)))
	stopifnot(is.finite(res$runtime))
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
	instance = inst, estimator = "kernel", n_coalitions = 16L,
	kernel_variant = "original", sage_n_samples = 20, sampler = "gaussian"
)
stopifnot(nrow(cres$importance[[1]]) == 3L)

cat("OK: SAGE algorithm functions\n")
```

- [ ] **Step 2: Run it to verify it fails**

```sh
Rscript tests/test-sage-algorithms.R
```

Expected: FAIL with `unused argument (estimator = "permutation")` — the current `algo_MarginalSAGE` has no `estimator` parameter.

- [ ] **Step 3: Rewrite `algo_MarginalSAGE`**

Replace `R/algorithms.R:127-171` entirely:

```r
algo_MarginalSAGE <- function(
	data = NULL,
	job = NULL,
	instance,
	estimator = "permutation",
	n_permutations = NA_integer_,
	n_coalitions = NA_integer_,
	kernel_variant = NA_character_,
	sage_n_samples = 200,
	batch_size = 10000,
	early_stopping = FALSE,
	min_permutations = 20
) {
	# Create learner for this algorithm
	learner <- create_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		task = instance$task
	)

	args <- list(
		task = instance$task,
		learner = learner,
		measure = instance$measure,
		resampling = instance$resampling,
		estimator = estimator,
		n_samples = sage_n_samples,
		batch_size = batch_size
	)

	# Each estimator owns exactly one budget argument and the others must stay
	# unset: n_permutations with estimator = "kernel" is a hard error, and the
	# permutation-only convergence controls warn on the other estimators. The
	# design table carries NA in the inapplicable columns, so they are read here
	# only inside their own branch.
	if (estimator == "permutation") {
		args$n_permutations <- as.integer(n_permutations)
		args$early_stopping <- early_stopping
		args$min_permutations <- as.integer(min_permutations)
	} else if (estimator == "kernel") {
		args$n_coalitions <- as.integer(n_coalitions)
		args$kernel_variant <- as.character(kernel_variant)
	}

	method <- do.call(MarginalSAGE$new, args)

	start_time <- Sys.time()
	method$compute()
	end_time <- Sys.time()

	# The exact estimator has no coalition-sampling error and rejects
	# ci_method = "montecarlo"; the sampling estimators report the Monte Carlo
	# SEs that the validation analysis checks for calibration.
	importance <- if (estimator == "exact") {
		method$importance()
	} else {
		method$importance(ci_method = "montecarlo")
	}

	data.table::data.table(
		importance = list(importance),
		scores = list(method$scores()),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = method$resample_result$aggregate(instance$measure_eval),
		n_permutations_used = method$n_permutations_used,
		converged = method$converged,
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}
```

- [ ] **Step 4: Rewrite `algo_ConditionalSAGE`**

Replace `R/algorithms.R:178-227` entirely:

```r
algo_ConditionalSAGE <- function(
	data = NULL,
	job = NULL,
	instance,
	estimator = "permutation",
	n_permutations = NA_integer_,
	n_coalitions = NA_integer_,
	kernel_variant = NA_character_,
	sage_n_samples = 200,
	sampler = "arf",
	batch_size = 10000,
	early_stopping = FALSE,
	min_permutations = 20
) {
	# Create learner for this algorithm
	learner <- create_learner(
		learner_type = instance$learner_type,
		task_type = instance$task_type,
		task = instance$task
	)

	# Create sampler instance
	sampler_instance <- create_sampler(sampler = sampler, task = instance$task)

	args <- list(
		task = instance$task,
		learner = learner,
		measure = instance$measure,
		resampling = instance$resampling,
		sampler = sampler_instance,
		estimator = estimator,
		n_samples = sage_n_samples,
		batch_size = batch_size
	)

	# See algo_MarginalSAGE: budget arguments are mutually exclusive, and the
	# permutation-only convergence controls warn on the other estimators.
	if (estimator == "permutation") {
		args$n_permutations <- as.integer(n_permutations)
		args$early_stopping <- early_stopping
		args$min_permutations <- as.integer(min_permutations)
	} else if (estimator == "kernel") {
		args$n_coalitions <- as.integer(n_coalitions)
		args$kernel_variant <- as.character(kernel_variant)
	}

	method <- do.call(ConditionalSAGE$new, args)

	start_time <- Sys.time()
	method$compute()
	end_time <- Sys.time()

	importance <- if (estimator == "exact") {
		method$importance()
	} else {
		method$importance(ci_method = "montecarlo")
	}

	data.table::data.table(
		importance = list(importance),
		scores = list(method$scores()),
		runtime = as.numeric(difftime(end_time, start_time, units = "secs")),
		learner_performance = method$resample_result$aggregate(instance$measure_eval),
		n_permutations_used = method$n_permutations_used,
		converged = method$converged,
		n_features = instance$n_features,
		n_samples = instance$n_samples,
		task_type = instance$task_type
	)
}
```

- [ ] **Step 5: Run the smoke test to verify it passes**

```sh
Rscript tests/test-sage-algorithms.R
```

Expected:
```
   permutation ok
   kernel_orig ok
   kernel_unb ok
   exact ok
OK: SAGE algorithm functions
```

- [ ] **Step 6: Commit**

```sh
air format R/algorithms.R tests/test-sage-algorithms.R
git add R/algorithms.R tests/test-sage-algorithms.R
git commit -m "feat: add estimator axis to xplainfi SAGE algorithm functions

Constructor arguments are assembled conditionally and dispatched via
do.call, because the budget arguments are mutually exclusive and the
permutation-only convergence controls warn on the other estimators.
Sampling estimators now store montecarlo confidence intervals; exact
rejects that ci_method and stores point estimates."
```

---

### Task 4: Python `sage` reference arm gains the estimator axis

Extends the existing `MarginalSAGE_sage` algorithm rather than adding a new one. A name like `MarginalSAGE_sage_perm` would not match `.reference_suffix` (`_(iml|vip|fippy|sage)$` in `R/provenance.R`), so it would be misclassified as an xplainfi method *and* would miss the `python` submission tag from `algo_is_python()` — which is what keeps Python jobs out of R torch chunks.

**Files:**
- Modify: `R/algorithms.R:908-1012` (`algo_MarginalSAGE_sage`)
- Modify: `tests/test-sage-algorithms.R` (append the reference-arm case)

**Interfaces:**
- Consumes: nothing from Tasks 2-3.
- Produces: `algo_MarginalSAGE_sage(data, job, instance, estimator, n_permutations, n_coalitions, sage_n_samples, early_stopping)` returning the existing result `data.table`. Task 6 registers it against `sage_algo_design(conf, estimators = c("permutation", "kernel"), kernel_variants = NA_character_)`.

Verified `sage` 0.0.6 signatures:
```
KernelEstimator.__init__(self, imputer, loss='cross entropy', random_state=None)
KernelEstimator.__call__(self, X, Y=None, batch_size=512, detect_convergence=True,
                         thresh=0.025, n_samples=None, verbose=False, bar=True, check_every=5)
PermutationEstimator.__init__(self, imputer, loss='cross entropy', n_jobs=1, random_state=None)
PermutationEstimator.__call__(self, X, Y=None, batch_size=512, detect_convergence=True,
                              thresh=0.025, n_permutations=None, min_coalition=0.0,
                              max_coalition=1.0, verbose=False, bar=True)
```

**Naming trap:** `KernelEstimator.__call__(n_samples=)` is the *coalition* budget. Our `sage_n_samples` is the marginalization background size, which in `sage` is the `MarginalImputer(data=)` argument. Wiring `sage_n_samples` into `n_samples` would make the reference arm's budget track the wrong axis and produce a plausible-but-wrong disagreement with xplainfi. The mapping is `n_coalitions -> n_samples` and `sage_n_samples -> MarginalImputer(data =)`.

- [ ] **Step 1: Append the failing reference-arm case to the smoke test**

Add to the end of `tests/test-sage-algorithms.R`, before the final `cat()`:

```r
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
```

- [ ] **Step 2: Run it to verify it fails**

```sh
Rscript tests/test-sage-algorithms.R
```

Expected: the four xplainfi cases pass, then FAIL with `unused argument (estimator = est)` — the current `algo_MarginalSAGE_sage` has no `estimator` parameter.

- [ ] **Step 3: Replace the estimator construction and call**

In `R/algorithms.R`, change the `algo_MarginalSAGE_sage` signature from:

```r
algo_MarginalSAGE_sage <- function(
	data = NULL,
	job = NULL,
	instance,
	sage_n_samples = 200, # Background data size for marginalization
	early_stopping = TRUE,
	min_permutations = 20
) {
```

to:

```r
algo_MarginalSAGE_sage <- function(
	data = NULL,
	job = NULL,
	instance,
	estimator = "kernel",
	n_permutations = NA_integer_,
	n_coalitions = NA_integer_,
	sage_n_samples = 200, # Background data size for marginalization
	early_stopping = FALSE,
	min_permutations = 20
) {
```

Then replace the estimator construction block (currently `estimator <- sage$KernelEstimator(...)`, which shadows the new argument name) with:

```r
	# `estimator` is now the design column, so the Python object gets its own name.
	# PermutationEstimator exposes n_jobs; KernelEstimator has no thread argument.
	# Thread parity is mandatory (see CLAUDE.md) -- an unset n_jobs would give the
	# reference arm one core while xplainfi gets n_threads().
	estimator_obj <- switch(
		estimator,
		kernel = sage$KernelEstimator(
			imputer = imputer,
			loss = loss,
			random_state = as.integer(random_state)
		),
		permutation = sage$PermutationEstimator(
			imputer = imputer,
			loss = loss,
			random_state = as.integer(random_state),
			n_jobs = as.integer(n_threads())
		),
		cli::cli_abort("Unsupported {.arg estimator} for the sage package: {.val {estimator}}")
	)
```

And replace the `explanation <- estimator(...)` call with:

```r
	np <- reticulate::import("numpy", convert = FALSE)

	call_args <- list(
		X = np$array(sklearn_data$X_test),
		Y = np$array(sklearn_data$y_test),
		detect_convergence = early_stopping,
		verbose = FALSE,
		bar = FALSE
	)

	# sage's own budget arguments. NOTE: `n_samples` on KernelEstimator is the
	# COALITION budget, not the background sample -- that is MarginalImputer(data=)
	# above, which already consumes sage_n_samples.
	if (estimator == "kernel") {
		call_args$n_samples <- as.integer(n_coalitions)
	} else {
		call_args$n_permutations <- as.integer(n_permutations)
	}

	explanation <- do.call(estimator_obj, call_args)
```

Leave the surrounding code (data conversion, learner fitting, `learner_performance`, `imputer`, `loss`, `random_state`, importance extraction, return table) exactly as it is.

- [ ] **Step 4: Run the smoke test to verify it passes**

```sh
Rscript tests/test-sage-algorithms.R
```

Expected: all four xplainfi cases, then `sage kernel ok` and `sage permutation ok`, then `OK: SAGE algorithm functions`.

- [ ] **Step 5: Commit**

```sh
air format R/algorithms.R tests/test-sage-algorithms.R
git add R/algorithms.R tests/test-sage-algorithms.R
git commit -m "feat: add estimator axis to the Python sage reference arm

Extends MarginalSAGE_sage rather than adding MarginalSAGE_sage_perm,
which would not match .reference_suffix and would be misclassified as an
xplainfi method and miss the python submission tag.

Also fixes two pre-existing issues: the arm ran unbudgeted at whatever
its convergence detector picked, and PermutationEstimator's n_jobs would
have defaulted to 1 while xplainfi gets n_threads()."
```

---

### Task 5: Configuration knobs for both lanes

**Files:**
- Modify: `importance/config.R:21-51` (the `conf` list)
- Modify: `runtime/config.R:21-50` (the `conf` list)

**Interfaces:**
- Consumes: nothing.
- Produces: `conf$n_coalitions`, `conf$kernel_variants`, `conf$sage_estimators`, `conf$methods` in both lanes, plus `conf$min_permutations` in the runtime lane. Task 6 reads all of them.

- [ ] **Step 1: Update `importance/config.R`**

Inside the `conf <- list(...)` call, apply these changes:

```r
	seed = 2025,
	repls = 10,
	# Samples to generate
	n_samples = 5000,
	# Affects correlation task
	correlation = c(0.2, 0.5, 0.9),
	# Affects PFI and CFI
	n_repeats = 100,
	# SAGE permutation-estimator budget. Early stopping is off for this run so
	# the permutation arm spends a known budget and stays comparable, per
	# evaluated coalition, against the kernel and exact arms.
	n_permutations = c(10, 50, 100),
	min_permutations = 20,
	sage_early_stopping = FALSE,
	# SAGE kernel-estimator budget (paired coalition draws). Independent of
	# n_features, so evaluated-coalition cost differs across problems -- the
	# analysis reports cost explicitly rather than matching it in the design.
	n_coalitions = c(32, 128, 512),
	# Design-matrix ("A matrix") variant: "original" samples it alongside the
	# right-hand side (Covert & Lee Eq. 7), "unbiased" uses the exact closed form
	# (Eq. 9) and is what the Python sage package implements.
	kernel_variants = c("original", "unbiased"),
	# "exact" enumerates all 2^n_features coalitions -- the ground truth this run
	# validates the sampling estimators against.
	sage_estimators = c("permutation", "kernel", "exact"),
	# Size of sampled data used for Monte Carlo integration in SAGE methods, 200 was usually sufficient
	# increases RAM usage a lot if set too high, and returns are diminishing somewhat quickly
	sage_n_samples = c(100),
	# SAGE-only dev validation run (mlr-org/xplainfi#83): PFI/CFI/LOCO and their
	# reference implementations sit this out. Restore the full list to re-enable.
	methods = c(
		"MarginalSAGE",
		"ConditionalSAGE",
		"MarginalSAGE_sage",
		"MarginalSAGE_fippy",
		"ConditionalSAGE_fippy"
	),
```

Leave `learner_types` and `samplers` as they currently stand (already trimmed to `c("linear", "rf")` and `"gaussian"`).

- [ ] **Step 2: Update `runtime/config.R`**

Inside its `conf <- list(...)`, apply:

```r
	seed = 2025,
	repls = 10,
	# ... n_samples, n_features, n_repeats unchanged ...
	# For SAGE permutations
	n_permutations = c(10, 50, 100),
	min_permutations = 20,
	sage_early_stopping = FALSE,
	n_coalitions = c(32, 128, 512),
	kernel_variants = c("original", "unbiased"),
	sage_estimators = c("permutation", "kernel", "exact"),
	# Size of sampled data used for Monte Carlo integration in SAGE methods.
	# Two values so the marginalization budget contributes visible variance to
	# the cost curve; the importance lane holds it fixed instead.
	sage_n_samples = c(10, 50),
	# SAGE-only dev validation run (mlr-org/xplainfi#83).
	methods = c(
		"MarginalSAGE",
		"ConditionalSAGE",
		"MarginalSAGE_sage",
		"MarginalSAGE_fippy",
		"ConditionalSAGE_fippy"
	),
```

`min_permutations` is new here — the runtime lane's SAGE designs never set it, and `sage_algo_design()` reads it unconditionally. A missing `conf$min_permutations` would silently drop the column from `CJ()` rather than error.

- [ ] **Step 3: Verify both configs load and carry the new knobs**

```sh
Rscript -e 'source("importance/config.R"); str(conf[c("repls","n_coalitions","kernel_variants","sage_estimators","min_permutations","methods")])'
Rscript -e 'source("runtime/config.R"); str(conf[c("repls","n_coalitions","kernel_variants","sage_estimators","min_permutations","sage_n_samples")])'
```

Expected: `repls` is `10` in both; `n_coalitions` is `32 128 512`; `min_permutations` is `20` in both; the runtime lane's `sage_n_samples` is `10 50`.

- [ ] **Step 4: Verify the design builder produces the expected row counts with the real configs**

```sh
Rscript -e 'source("setup-common.R"); source("importance/config.R"); cat(nrow(sage_algo_design(conf)), "\n")'
Rscript -e 'source("setup-common.R"); source("runtime/config.R"); cat(nrow(sage_algo_design(conf)), "\n")'
```

Expected: `10` (importance, one `sage_n_samples`) and `20` (runtime, two `sage_n_samples`).

- [ ] **Step 5: Commit**

```sh
air format importance/config.R runtime/config.R
git add importance/config.R runtime/config.R
git commit -m "config: add SAGE estimator knobs and a methods filter

repls = 10 in both lanes -- this is a development sanity check, not an
exhaustive study. Early stopping is off so the permutation arm spends a
known budget. conf\$methods parks PFI/CFI/LOCO non-destructively and
composes with the existing XPLAINFI_BENCH_PROVIDERS filter."
```

---

### Task 6: Wire the estimator axis into both registries

**Files:**
- Modify: `importance/setup-batchtools.R:71-75` (provider selection), `:106-235` (`algo_designs`), `:250-268` (exclusions)
- Modify: `runtime/setup-batchtools.R:57-62` (provider selection), `:81-155` (`algo_designs`), `:170-200` (exclusions)

**Interfaces:**
- Consumes: `sage_algo_design()` (Task 2), the algorithm functions (Tasks 3-4), the config knobs (Task 5).
- Produces: two populated registries at `registries/<lane>/xplainfi-1.1.0.9000-kernelsage/`. Tasks 7-8 read them.

- [ ] **Step 1: Apply the methods filter in both files**

In **both** `setup-batchtools.R` files, immediately after the existing `active_algos <- select_algorithms(...)` line and before the `cli::cli_alert_info(...)` that reports it, insert:

```r
# Second, independent filter: which methods this run is about at all. Composes
# with the provider filter above -- providers select implementations, methods
# select the importance measures.
active_algos <- intersect(active_algos, conf$methods)
```

- [ ] **Step 2: Replace the SAGE entries in `importance/setup-batchtools.R`'s `algo_designs`**

Delete the `MarginalSAGE`, `ConditionalSAGE`, `MarginalSAGE_fippy`, `ConditionalSAGE_fippy` and `MarginalSAGE_sage` entries and put in their place:

```r
	# SAGE estimator axis. The three estimators take mutually exclusive budget
	# arguments, so these designs are an rbind of per-estimator sub-designs with
	# NA in the inapplicable columns -- see sage_algo_design() in R/helpers.R.
	MarginalSAGE = sage_algo_design(conf),

	ConditionalSAGE = sage_algo_design(conf, sampler = conf$samplers),

	# Python sage: kernel + permutation. Its kernel estimator is always the
	# unbiased variant, so it has no variant choice, and it has no exact arm.
	MarginalSAGE_sage = sage_algo_design(
		conf,
		estimators = c("permutation", "kernel"),
		kernel_variants = NA_character_
	),

	# fippy implements the permutation estimator only. `estimator` is set
	# explicitly so the column is present on every SAGE row and the analysis
	# join stays uniform across arms.
	MarginalSAGE_fippy = sage_algo_design(
		conf,
		sampler = "simple",
		estimators = "permutation"
	),

	ConditionalSAGE_fippy = sage_algo_design(
		conf,
		sampler = "gaussian",
		estimators = "permutation"
	),
```

Leave the `PFI`, `CFI`, `LOCO`, `PFI_iml`, `PFI_vip`, `PFI_fippy`, `CFI_fippy` entries in place — the existing `algo_designs <- algo_designs[active_algos]` line already drops them, and leaving them intact keeps the diff against `main` small.

- [ ] **Step 3: Apply the identical replacement in `runtime/setup-batchtools.R`**

Same five entries, same code. `conf$samplers` is `"gaussian"` in that lane.

- [ ] **Step 4: Add the exact-estimator exclusion to `importance/setup-batchtools.R`**

In the "Remove incompatible sampler-task combinations" section, after the existing `featureless_non_xplainfi_jobs` block, add:

```r
# ============================================================================
# Remove infeasible exact-estimator jobs
# ============================================================================

# estimator = "exact" enumerates 2^n_features coalitions and aborts above
# max_features (12L). bike_sharing has 13 features. friedman1 (10 features,
# 1024 coalitions) is kept: that is essentially the cost of the largest kernel
# budget (2 + 2 * 512 = 1026), so it is a fair ground truth rather than an
# outlier expense.
exact_infeasible <- unwrap(getJobTable())[
	estimator == "exact" & problem == "bike_sharing",
]

if (nrow(exact_infeasible) > 0) {
	cli::cli_alert_warning(
		"Removing {nrow(exact_infeasible)} exact-estimator job(s) on tasks above max_features"
	)
	removeExperiments(exact_infeasible)
}
```

- [ ] **Step 5: Add the exclusion and scope the pruner in `runtime/setup-batchtools.R`**

Add the equivalent exclusion (the `peak` task at 25 features):

```r
# ============================================================================
# Remove infeasible exact-estimator jobs
# ============================================================================

# estimator = "exact" enumerates 2^n_features coalitions and aborts above
# max_features (12L). Keeps n_features 5 (32 coalitions) and 10 (1024).
exact_infeasible <- unwrap(getJobTable())[
	estimator == "exact" & n_features > 12,
]

if (nrow(exact_infeasible) > 0) {
	cli::cli_alert_warning(
		"Removing {nrow(exact_infeasible)} exact-estimator job(s) above max_features"
	)
	removeExperiments(exact_infeasible)
}
```

Then scope the existing early-stopping pruner to the permutation estimator. Change:

```r
sage_early_stopping <- unwrap(getJobTable())[
	early_stopping & n_permutations < max(n_permutations, na.rm = TRUE),
]
```

to:

```r
# Scoped to the permutation estimator: kernel/exact rows carry NA budgets, and
# early_stopping is permutation-only. (With sage_early_stopping = FALSE this is
# a no-op, but leaving it unscoped is a trap if early stopping is turned back on.)
sage_early_stopping <- unwrap(getJobTable())[
	estimator == "permutation" &
		early_stopping &
		n_permutations < max(n_permutations, na.rm = TRUE),
]
```

- [ ] **Step 6: Build the importance registry and verify its shape**

```sh
rm -rf registries/importance/xplainfi-1.1.0.9000-kernelsage
Rscript importance/setup-batchtools.R
```

Then verify:

```sh
Rscript -e '
source("setup-common.R"); source("importance/config.R")
library(batchtools); library(data.table)
reg <- loadRegistry(conf$reg_path, work.dir = here::here())
tab <- unwrap(getJobTable())
cat("total:", nrow(tab), "\n")
print(tab[, .N, by = .(algorithm, estimator)][order(algorithm, estimator)])
stopifnot(!any(tab$algorithm %in% c("PFI", "CFI", "LOCO", "PFI_iml", "PFI_vip")))
stopifnot(nrow(tab[estimator == "exact" & problem == "bike_sharing"]) == 0)
stopifnot(nrow(tab[estimator == "kernel" & !is.na(n_permutations)]) == 0)
stopifnot(nrow(tab[estimator == "permutation" & !is.na(n_coalitions)]) == 0)
cat("shape OK\n")
'
```

Expected: `MarginalSAGE` and `ConditionalSAGE` at **3,960** jobs combined — 20 problem-design rows × 10 algo rows × 2 algorithms × 10 repls = 4,000, minus 40 for the removed `bike_sharing` exact rows (2 problem-design rows × 1 exact row × 2 algorithms × 10 repls). Plus the Python arms. No PFI/CFI/LOCO rows; `shape OK`.

The 20 problem-design rows break down as: `friedman1` 2, `bike_sharing` 2, `correlated` 6 (3 correlations × 2 learners), and `ewald` / `interactions` / `independent` / `confounded` / `mediated` 2 each.

- [ ] **Step 7: Build the runtime registry and verify its shape**

```sh
rm -rf registries/runtime/xplainfi-1.1.0.9000-kernelsage
Rscript runtime/setup-batchtools.R
Rscript -e '
source("setup-common.R"); source("runtime/config.R")
library(batchtools); library(data.table)
reg <- loadRegistry(conf$reg_path, work.dir = here::here())
tab <- unwrap(getJobTable())
cat("total:", nrow(tab), "\n")
print(tab[, .N, by = .(algorithm, estimator)][order(algorithm, estimator)])
stopifnot(nrow(tab[estimator == "exact" & n_features > 12]) == 0)
stopifnot(nrow(tab[estimator == "exact" & n_features == 10]) > 0)
cat("shape OK\n")
'
```

Expected: no exact jobs at `n_features = 25`, some at `n_features = 10`; `shape OK`.

- [ ] **Step 8: Verify the Python jobs are tagged for backend isolation**

```sh
Rscript -e '
source("setup-common.R"); source("importance/config.R")
library(batchtools); library(data.table)
reg <- loadRegistry(conf$reg_path, work.dir = here::here())
py <- findTagged("python")
tab <- unwrap(getJobTable())
stopifnot(nrow(py) > 0)
stopifnot(all(tab[job.id %in% py$job.id, unique(algorithm)] %in%
	c("MarginalSAGE_sage", "MarginalSAGE_fippy", "ConditionalSAGE_fippy")))
stopifnot(nrow(tab[algorithm == "MarginalSAGE_sage" & !(job.id %in% py$job.id)]) == 0)
cat("python tagging OK:", nrow(py), "jobs\n")
'
```

Expected: every `MarginalSAGE_sage` job (both estimators) carries the `python` tag, so `plan_submission()` keeps them out of R torch chunks.

- [ ] **Step 9: Commit**

```sh
air format importance/setup-batchtools.R runtime/setup-batchtools.R
git add importance/setup-batchtools.R runtime/setup-batchtools.R
git commit -m "feat: wire the SAGE estimator axis into both registries

Applies conf\$methods, replaces the five SAGE algo_designs with
sage_algo_design() calls, and drops exact-estimator jobs above
max_features (bike_sharing at 13 features, peak at 25). The runtime
lane's early-stopping pruner is scoped to the permutation estimator so
it cannot touch NA-budget kernel/exact rows."
```

---

### Task 7: The validation analysis

**Files:**
- Create: `importance/analysis-kernel-sage.R`

**Interfaces:**
- Consumes: `load_reduced()` / `latest_reduced()` from `R/provenance.R`, and reduced tables carrying `estimator`, `kernel_variant`, `n_coalitions`, `n_permutations`, `se`, `conf_lower`, `conf_upper`.
- Produces: three printed summary tables and `results/importance/kernel-sage-validation.rds`.

- [ ] **Step 1: Write the analysis script**

Create `importance/analysis-kernel-sage.R`:

```r
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
res[, n_evals := fcase(
	estimator == "permutation", 1 + n_permutations * n_features,
	estimator == "kernel", 2 + 2 * n_coalitions,
	estimator == "exact", 2^n_features
)]

# A readable label for the estimator configuration.
res[, arm := fcase(
	estimator == "kernel", paste0("kernel-", kernel_variant),
	default = estimator
)]

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
```

- [ ] **Step 2: Verify it parses and fails cleanly with no results present**

```sh
Rscript importance/analysis-kernel-sage.R
```

Expected: a clean abort — `No reduced importance results found. Run collect-results.R first.` A syntax error or any other message is a failure.

- [ ] **Step 3: Commit**

```sh
air format importance/analysis-kernel-sage.R
git add importance/analysis-kernel-sage.R
git commit -m "feat: add kernel SAGE validation analysis

Three checks joined on the semantic paired key, never job.id: bias
against the exact arm, montecarlo CI coverage of the exact value, and
xplainfi kernel-unbiased against the Python sage package (the only
apples-to-apples pairing, since sage implements exactly that variant)."
```

---

### Task 8: Pilot run, runtime estimates, full submission

`estimateRuntimes()` fits a random forest on **finished** jobs, so a fresh registry gives `eta.R` nothing to learn from. The estimator axis is entirely new — no historical registry has an `estimator` column — so there is no prior model to borrow either. Hence: pilot, then estimate, then submit the rest.

**Files:**
- No source changes. This task runs the pipeline.

**Interfaces:**
- Consumes: both registries from Task 6, the analysis from Task 7.
- Produces: `eta-importance.rds`, `eta-runtime.rds`, `results/importance/*.rds`, `results/runtime/*.rds`.

- [ ] **Step 1: Confirm all three dependency backends are in sync**

```sh
make check
```

Expected: `OK` for R packages, `.venv`, and libtorch. Resolve any `TODO` before submitting — a missing backend fails on the compute nodes, not here.

- [ ] **Step 2: Submit the importance-lane pilot (`repl = 1` only)**

```sh
Rscript -e '
source("setup-common.R"); source("importance/config.R")
library(batchtools)
reg <- loadRegistry(conf$reg_path, work.dir = here::here(), writeable = TRUE)
ids <- findExperiments(repls = 1)
cat("pilot jobs:", nrow(ids), "\n")
groups <- plan_submission(ids)
for (g in groups) submitJobs(g$jobs, resources = g$resources)
'
```

`repl = 1` covers every design cell exactly once, which is the coverage the runtime model needs — extrapolating to an unseen cell is what produces bad walltime tiers. With no estimates present, `plan_submission()` falls back to chunking by job count, which is fine at pilot size.

- [ ] **Step 3: Wait for the pilot and inspect the cost outliers**

```sh
Rscript -e '
source("setup-common.R"); source("importance/config.R")
library(batchtools); library(data.table)
reg <- loadRegistry(conf$reg_path, work.dir = here::here())
getStatus()
tab <- merge(unwrap(getJobTable()), getJobStatus()[, .(job.id, time.running)], by = "job.id")
print(tab[!is.na(time.running), .(n = .N, median_s = as.numeric(median(time.running))),
	by = .(algorithm, estimator, problem)][order(-median_s)][1:15])
'
```

The exact arm on `friedman1` (1024 coalitions) is the expected outlier. If it dominates the lane, drop it before committing the remaining 9 replications by removing those experiments — it is a ground truth for the 10-feature case only, and the 3-to-5-feature problems already provide one.

- [ ] **Step 4: Fit the runtime model**

```sh
Rscript importance/eta.R
```

Expected: prints an ETA table and `Runtime model R^2: <value>`, writes `eta-importance.rds`. An R² near zero means the model learned nothing useful; the submission will still work but will tier badly.

- [ ] **Step 5: Submit the remaining importance-lane replications**

```sh
Rscript importance/run-experiment.R
```

This resubmits everything outstanding (expired + never-run), grouped by backend then resource tier, with expired jobs' memory doubled via `escalate_memory()`.

- [ ] **Step 6: Repeat Steps 2-5 for the runtime lane**

Same commands with `runtime/config.R`, `runtime/eta.R`, `runtime/run-experiment.R`.

- [ ] **Step 7: Collect and analyse**

```sh
Rscript importance/collect-results.R
Rscript importance/analysis-kernel-sage.R
```

Expected: `collect-results.R` writes `results/importance/xplainfi+reference-v1.1.0.9000-kernelsage.rds`, and the analysis prints its three tables.

- [ ] **Step 8: Commit the durable results**

Registries are disposable scratch; the reduced tables are the versioned artifact.

```sh
git add results/importance results/runtime
git commit -m "results: kernel SAGE validation run (xplainfi 1.1.0.9000-kernelsage)"
```

---

## Verification Summary

Every check in this plan, runnable in order:

```sh
Rscript tests/check-kernel-sage-api.R      # Task 1: installed API matches the PR
Rscript tests/test-sage-algo-design.R      # Task 2: design shape, NA placement
Rscript tests/test-sage-algorithms.R       # Tasks 3-4: all estimators run end to end
```

Registry shape assertions live inline in Task 6 Steps 6-8.
