# Kernel SAGE validation — design

Date: 2026-07-21
Branch: `kernel-sage`
Upstream: [mlr-org/xplainfi#83](https://github.com/mlr-org/xplainfi/pull/83) (`kernel-sage`, `334b59ef`)

## Goal

Validate xplainfi's in-development Kernel SAGE estimator before release, on
correctness (importance lane) and cost scaling (runtime lane). This is a
development sanity check, not a release comparison: PFI, CFI and LOCO sit it out
so the whole budget goes to SAGE.

## What the PR adds

`MarginalSAGE` and `ConditionalSAGE` gain an `estimator` argument:

| `estimator` | Budget argument | Evaluated coalitions |
| --- | --- | --- |
| `"permutation"` (default, prior behaviour) | `n_permutations` | `1 + n_permutations * n_features` |
| `"kernel"` (Covert & Lee 2021) | `n_coalitions` | `2 + 2 * n_coalitions` |
| `"exact"` | none | `2^n_features` |

Constraints that shape the design:

- The budget arguments are **mutually exclusive**. Setting another estimator's
  budget is a hard error, so the estimator axis cannot be a `CJ()`.
- `early_stopping`, `min_permutations`, `se_threshold` and `check_interval` are
  permutation-only; a non-default value for another estimator warns.
- `kernel_variant` selects how the design matrix ("A matrix") is obtained:
  `"original"` (Eq. 7, sampled design matrix, xplainfi default) and
  `"unbiased"` (Eq. 9, exact closed-form design matrix — the variant the Python
  `sage` package implements, and therefore the only apples-to-apples pairing
  against it).
- `estimator = "exact"` aborts above `max_features = 12L`.
- `$importance(ci_method = "montecarlo")` is new: Wald intervals from the
  coalition-sampling standard errors. The kernel estimator obtains these via a
  multivariate delta method. The exact estimator has no coalition-sampling error
  and rejects this method.

Feature counts, which govern exact-estimator feasibility:

| Problem | Features | `2^p` |
| --- | --- | --- |
| `confounded` | 3 | 8 |
| `correlated`, `mediated` | 4 | 16 |
| `ewald`, `interactions`, `independent` | 5 | 32 |
| `friedman1` | 10 | 1024 |
| `bike_sharing` | 13 | — (over `max_features`) |
| `peak` (runtime lane) | 5 / 10 / 25 | 32 / 1024 / — |

## Decisions

| Decision | Choice | Rationale |
| --- | --- | --- |
| Kernel budget | Independent grid `n_coalitions = c(32, 128, 512)` | Simple, readable design table; costs are not matched across problems with different `p`, which the analysis accounts for by reporting evaluated coalitions. |
| Variants in scope | xplainfi Marginal + Conditional × {permutation, kernel-original, kernel-unbiased, exact}; Python `sage` × {kernel, permutation} | Separates estimator differences from implementation differences on both sides. |
| Disabling PFI/CFI/LOCO | `conf$methods` knob in `config.R` | Non-destructive, one line to revert, composes with the existing `XPLAINFI_BENCH_PROVIDERS` filter, and avoids a merge-conflict magnet in `setup-batchtools.R` against `main`. |
| Uncertainty | Store `$importance(ci_method = "montecarlo")` | The only way to validate the new delta-method SE machinery rather than just the point estimates. |
| Scale | `repls = 10` in both lanes; all 8 problems in the importance lane | ~4,000 xplainfi jobs (importance) and ~6,000 (runtime). This is a development sanity check, not an exhaustive study: full DGP coverage matters more than Monte Carlo precision when hunting estimator bugs in conditioning/confounding/mediation structure. |
| Marginalization axis | `sage_n_samples = c(100)` (importance), `c(10, 50)` (runtime) | The runtime lane needs *some* variance in the marginalization budget to show its cost contribution; two distinct values suffice. The importance lane holds it fixed so the coalition-budget axis is read cleanly. |
| Permutation early stopping | Off in the importance lane | With early stopping the permutation arm spends an unknown budget, making accuracy-vs-cost against kernel unreadable. |
| Runtime lane | Same estimator axis; exact only where `p <= 10` | Gives the scaling curve in `p` and `n`; exact is skipped at `p = 25`, which exceeds `max_features`. |

## 1. Dependency and registry namespacing

`rproject.toml` — point xplainfi at the PR branch, leave every reference
dependency untouched:

```toml
{ name = "xplainfi", git = "https://github.com/mlr-org/xplainfi", branch = "kernel-sage" },
```

Then `make r-deps`, and confirm `packageVersion("xplainfi")` plus the resolved
SHA in `rv.lock` (the source of truth `provenance.rds` reads).

The PR branch reports version `1.1.0.9000` — the same string a `main`-tracking
dev install reports, so registries and reduced tables would silently collide.
This run therefore uses an explicit namespace tag via the existing env var (no
code change; `config.R` already reads it):

```sh
export XPLAINFI_BENCH_VERSION=1.1.0.9000-kernelsage
export XPLAINFI_BENCH_PROVIDERS=all
```

giving `registries/<lane>/xplainfi-1.1.0.9000-kernelsage/` and
`results/<lane>/<provider>-v1.1.0.9000-kernelsage.rds`.

**`export` is required.** `.envrc` is gitignored and direnv is not installed on
this host, so the file is sourced by hand; a bare `FOO=bar` sets a shell
variable, not an environment variable, and `Sys.getenv()` would return `""`.
The failure is silent — it shows up only as the wrong registry path. Verify with
`Rscript -e 'Sys.getenv("XPLAINFI_BENCH_VERSION")'` before running
`setup-batchtools.R`.

## 2. Algorithm functions (`R/algorithms.R`)

### `algo_MarginalSAGE` / `algo_ConditionalSAGE`

New arguments `estimator`, `n_coalitions`, `kernel_variant`. Constructor
arguments are assembled conditionally and dispatched through `do.call`, because
passing an inapplicable budget argument is an error rather than a no-op:

```r
algo_MarginalSAGE <- function(
	data = NULL, job = NULL, instance,
	estimator = "permutation",
	n_permutations = NA_integer_,
	n_coalitions = NA_integer_,
	kernel_variant = NA_character_,
	sage_n_samples = 200,
	batch_size = 10000,
	early_stopping = FALSE,
	min_permutations = 20
) {
	args <- list(
		task = instance$task,
		learner = create_learner(...),
		measure = instance$measure,
		resampling = instance$resampling,
		estimator = estimator,
		n_samples = sage_n_samples,
		batch_size = batch_size
	)
	# Each estimator takes exactly one budget argument; the others must stay unset
	# (n_permutations with estimator = "kernel" is an error, not a no-op), and the
	# permutation-only convergence controls warn if set for kernel/exact.
	if (estimator == "permutation") {
		args$n_permutations <- as.integer(n_permutations)
		args$early_stopping <- early_stopping
		args$min_permutations <- as.integer(min_permutations)
	} else if (estimator == "kernel") {
		args$n_coalitions <- as.integer(n_coalitions)
		args$kernel_variant <- as.character(kernel_variant)
	}
	method <- do.call(MarginalSAGE$new, args)
	...
	# Exact has no coalition-sampling error and rejects ci_method = "montecarlo".
	imp <- if (estimator == "exact") {
		method$importance()
	} else {
		method$importance(ci_method = "montecarlo")
	}
	...
}
```

The `NA_integer_` / `NA_character_` defaults are load-bearing: the estimator axis
is built with `rbind(fill = TRUE)`, so inapplicable design columns arrive as `NA`,
and batchtools passes them positionally as `NA` rather than omitting them. The
values are read only inside their own branch.

`ConditionalSAGE` is identical plus its existing `sampler` argument.

The returned `data.table` is otherwise unchanged. `estimator` and
`kernel_variant` are not added to the result — they are job parameters and
`reduce_importances()` already merges `getJobPars()`. The extra `conf.low` /
`conf.high` columns from the montecarlo importance table flow through
`rbindlist(fill = TRUE)` unchanged.

### `algo_MarginalSAGE_sage`

Gains the same `estimator` axis instead of becoming a second algorithm. A name
like `MarginalSAGE_sage_perm` would not match `.reference_suffix`
(`_(iml|vip|fippy|sage)$`) and would be misclassified as an xplainfi method, and
would also miss the `python` submission tag from `algo_is_python()`.

```r
	estimator_obj <- switch(estimator,
		kernel = sage$KernelEstimator(imputer = imputer, loss = loss, random_state = rs),
		permutation = sage$PermutationEstimator(
			imputer = imputer, loss = loss, random_state = rs,
			n_jobs = as.integer(n_threads())
		)
	)
	call_args <- list(
		X = np$array(sklearn_data$X_test), Y = np$array(sklearn_data$y_test),
		detect_convergence = early_stopping, verbose = FALSE, bar = FALSE
	)
	# sage's own budget args: `n_samples` on the kernel estimator counts sampled
	# coalitions (NOT background rows -- that's the imputer's `data` argument).
	if (estimator == "kernel") {
		call_args$n_samples <- as.integer(n_coalitions)
	} else {
		call_args$n_permutations <- as.integer(n_permutations)
	}
	explanation <- do.call(estimator_obj, call_args)
```

Two pre-existing issues this fixes:

- The reference SAGE arm currently runs unbudgeted, at whatever its convergence
  detector picks. Passing an explicit budget with `detect_convergence = FALSE`
  makes it comparable to the xplainfi arms.
- `PermutationEstimator` takes `n_jobs`, which defaults to `1` while xplainfi
  gets `n_threads()` — a threading-parity violation of the kind CLAUDE.md warns
  about. `KernelEstimator` has no thread argument.

**Naming trap**: `KernelEstimator.__call__(n_samples=)` is the *coalition*
budget, while `conf$sage_n_samples` is the marginalization background size
(in `sage`, the `MarginalImputer(data=)` argument). Wiring `sage_n_samples` into
`n_samples` would make the reference arm's budget track the wrong axis and
produce a plausible-but-wrong disagreement with xplainfi. The mapping is
`n_coalitions -> n_samples` and `sage_n_samples -> MarginalImputer(data=)`.

## 3. Shared design builder (`R/helpers.R`)

One builder, used by both lanes, so the estimator axis cannot drift between them.
It is parameterised by which estimators and variants an implementation actually
supports, because the three arms differ:

- xplainfi `MarginalSAGE` / `ConditionalSAGE`: all three estimators, both kernel variants.
- Python `MarginalSAGE_sage`: kernel and permutation only, no variant choice
  (its kernel estimator is always the unbiased one).
- `fippy` arms: permutation only; `fippy` has no kernel estimator at all.

```r
# The three estimators take mutually exclusive budget arguments, so the estimator
# axis is an rbind of sub-designs (NA = "not applicable"), never a CJ.
sage_algo_design <- function(
	conf,
	sampler = NULL,
	estimators = conf$sage_estimators,
	kernel_variants = conf$kernel_variants
) {
	parts <- list()
	if ("permutation" %in% estimators) {
		parts$perm <- CJ(
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
		parts$kern <- CJ(
			estimator = "kernel",
			n_permutations = NA_integer_,
			n_coalitions = conf$n_coalitions,
			kernel_variant = kernel_variants,
			sage_n_samples = conf$sage_n_samples
		)
	}
	if ("exact" %in% estimators) {
		parts$exact <- CJ(
			estimator = "exact",
			n_permutations = NA_integer_,
			n_coalitions = NA_integer_,
			kernel_variant = NA_character_,
			sage_n_samples = conf$sage_n_samples
		)
	}
	d <- rbindlist(parts, fill = TRUE)
	if (!is.null(sampler)) {
		d <- as.data.table(merge(as.data.frame(d), data.frame(sampler = sampler)))
	}
	d[]
}
```

Rows per xplainfi algorithm at the chosen grid, per `sage_n_samples` value:
3 permutation + (2 variants × 3 `n_coalitions`) + 1 exact = **10**.
`MarginalSAGE_sage` gets 3 + 3 = **6**. The `fippy` arms get 3 each.

`min_permutations` is referenced unconditionally, so `runtime/config.R` gains it
too — the runtime lane's SAGE designs never set it today, and a missing
`conf$min_permutations` would silently drop the column from `CJ()` rather than
error.

## 4. Configuration

`importance/config.R`:

```r
repls = 10,                                             # was 50
n_permutations = c(10, 50, 100),                        # was 100
n_coalitions = c(32, 128, 512),                         # new
kernel_variants = c("original", "unbiased"),            # new
sage_estimators = c("permutation", "kernel", "exact"),  # new
sage_early_stopping = FALSE,                            # was TRUE: fixed budgets only
sage_n_samples = c(100),                                # unchanged
# SAGE-only dev validation run: PFI/CFI/LOCO and their references sit this out.
methods = c("MarginalSAGE", "ConditionalSAGE", "MarginalSAGE_sage",
            "MarginalSAGE_fippy", "ConditionalSAGE_fippy")
```

`runtime/config.R` gets the same new knobs and the same `methods` list, plus
`min_permutations = 20` (see below), and `repls = 10` (was 50). It keeps its own
`sage_n_samples = c(10, 50)`, which doubles its estimator axis to 20 rows per
xplainfi algorithm.

Resulting job counts:

| Lane | Problem-design rows | Algo rows / algorithm | xplainfi jobs |
| --- | --- | --- | --- |
| importance | 20 | 10 | 20 × 10 × 2 × 10 = **4,000** |
| runtime | 15 | 20 | 15 × 20 × 2 × 10 = **6,000** |

plus the Python reference arms (6 rows for `MarginalSAGE_sage`, 3 each for the
two `fippy` arms, on the same problem designs).

The `fippy` SAGE arms stay in: they are SAGE, already wired, and serve as a
second permutation-estimator reference.

## 5. Registry setup (both `setup-batchtools.R`)

One line after provider selection:

```r
active_algos <- intersect(active_algos, conf$methods)
```

The SAGE entries in `algo_designs` become:

```r
MarginalSAGE    = sage_algo_design(conf),
ConditionalSAGE = sage_algo_design(conf, sampler = conf$samplers),

# sage's kernel estimator is always the unbiased variant, and it has no exact arm.
MarginalSAGE_sage = sage_algo_design(
	conf, estimators = c("permutation", "kernel"), kernel_variants = NA_character_
),

# fippy implements the permutation estimator only. `estimator` is set explicitly
# so the column is present on every SAGE row and the analysis join stays uniform.
MarginalSAGE_fippy = sage_algo_design(
	conf, sampler = "simple", estimators = "permutation"
),
ConditionalSAGE_fippy = sage_algo_design(
	conf, sampler = "gaussian", estimators = "permutation"
)
```

Keeping the `fippy` arms permutation-only and at the existing `n_permutations`
grid means they remain comparable to the frozen reference results.

Post-`addExperiments` exclusions, since `estimator = "exact"` aborts above
`max_features = 12`:

- importance lane: drop `estimator == "exact" & problem == "bike_sharing"` (13 features).
- runtime lane: drop `estimator == "exact" & n_features == 25`.
- The runtime lane's existing early-stopping pruner is scoped to
  `estimator == "permutation"` so it cannot touch NA-budget kernel/exact rows.

`friedman1` **keeps** its exact arm: `2^10 = 1024` coalitions is essentially the
cost of the largest kernel budget (`2 + 2 * 512 = 1026`), so it is a fair ground
truth rather than an outlier expense.

Provider and `python` tagging is unchanged — both are derived from the algorithm
name, and the new Python permutation arm lives inside the existing
`MarginalSAGE_sage` algorithm, so it inherits the `python` tag and stays in a
separate submission group from the R jobs.

## 6. Analysis (`importance/analysis-kernel-sage.R`, new)

Joined on `c(problem, algorithm, learner_type, sampler, feature, repl)` plus
problem parameters — never `job.id`, which is registry-local. `estimator`,
`kernel_variant`, `n_coalitions` and `n_permutations` join as additional
parameter columns.

1. **Bias vs exact.** Per `(estimator, kernel_variant, budget)`, the paired
   difference against the `exact` arm on the same instance, plotted against
   evaluated coalitions. Expectation: kernel-original converges fastest,
   kernel-unbiased slower, both to zero.
2. **Monte Carlo CI calibration.** Coverage of the `montecarlo` interval for the
   exact value. This is the check that exercises the new delta-method SE
   machinery. The PR's NEWS notes that `sage`'s own covariance appears to deviate
   from Eq. 13 in the sign of the constraint-adjustment term, so a miscalibration
   here is a real finding in either direction.
3. **Cross-implementation.** xplainfi `kernel_variant = "unbiased"` against
   `MarginalSAGE_sage` (kernel) at matched coalition budgets — the only
   apples-to-apples pairing between the two packages. Point estimates should
   agree up to Monte Carlo error; reported uncertainties are *not* comparable
   (`sage` includes observation-sampling noise, xplainfi conditions on the test
   set).

## 7. Order of operations

1. `rproject.toml` -> `make r-deps`; confirm version and `rv.lock` SHA.
2. `export` the two env vars; verify with `Rscript -e 'Sys.getenv(...)'`.
3. `make check` (R, Python and libtorch sync) before submitting.
4. `Rscript importance/setup-batchtools.R`, then `runtime/setup-batchtools.R`.
5. **Pilot: submit `repl = 1` only.** `estimateRuntimes()` fits a random forest on
   *finished* jobs, so a fresh registry gives `eta.R` nothing to learn from. The
   estimator axis is entirely new — no historical registry has an `estimator`
   column — so there is no prior model to borrow either. Submit
   `findExperiments(repls = 1)` first (400 xplainfi jobs in the importance lane,
   600 in the runtime lane) and let it finish. `repl = 1` covers every design cell
   exactly once, which is the coverage the runtime model needs; extrapolating to
   an unseen cell is what produces bad walltime tiers. With no estimates present,
   `plan_submission()` falls back to chunking by job count, which is fine at pilot
   size.
6. `eta.R` per lane, now that the pilot has finished.
7. `run-experiment.R` per lane for the remaining replications.
8. `collect-results.R`, then `analysis-kernel-sage.R`.

The pilot doubles as a smoke test, and is the cheapest place to discover the two
cost outliers: the exact arm on `friedman1` (1024 coalitions) and `peak` at
`n_features = 10`. If either is disproportionate, drop it before committing the
remaining 9 replications. At `repls = 10` the pilot is a tenth of the total, so
it is cheap insurance rather than a meaningful fraction of the budget.

Note that `write_estimates()` takes an `ids` argument for exactly this shape of
workflow — `\(reg) findExperiments(repls = 1, reg = reg)` restricts the training
set explicitly, which matters if a later re-run mixes replications.

## Out of scope

- Comparing against previously released xplainfi versions. This validates the
  dev estimator against its own exact arm and the Python references, not against
  history.
- PFI, CFI, LOCO and their reference implementations. Re-enable by restoring
  `conf$methods`.
- An exact arm for `bike_sharing` (13 features) or `peak` at `n_features = 25`.
  Raising `max_features` is possible but costs `2^13 = 8192` coalitions, eight
  times the largest kernel budget.
