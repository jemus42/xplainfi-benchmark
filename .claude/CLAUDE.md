# xplainfi-benchmark

`batchtools` benchmark comparing **xplainfi**'s feature-importance methods against
reference implementations, on two axes: **results** (correctness) and **runtime**.

## Layout

- Two parallel lanes, `importance/` and `runtime/`, each with the same scripts:
  `config.R`, `setup-batchtools.R`, `run-experiment.R`, `collect-results.R`,
  `analysis.R`, `submit.R`, `eta.R`, `shiny.R`.
- `R/` shared: `helpers.R`, `helpers-python.R` (fippy/reticulate), `problems.R`
  (DGP generators `prob_*`), `algorithms.R` (`algo_*` fns), `plotting.R`,
  `provenance.R` (provider + versioning plumbing).
- `registries/<lane>/xplainfi-<version>/` — batchtools registries (gitignored, scratch).
- `results/<lane>/` — durable reduced tables (tracked).
- Deps: R via `rv` (`rproject.toml` + `rv.lock`, `rv sync`); Python via one shared
  `.venv` (uv) — deliberately not per-job ephemeral envs. Format R with `air format`
  (tabs, width 100). Files end with a newline.

## Providers and versioning

- **Provider is derived from the algorithm name by convention** (`R/provenance.R`):
  reference impls end in `_iml` / `_vip` / `_fippy` / `_sage`; everything else is
  xplainfi. Adding a method (e.g. a kernel SAGE estimator) needs no bookkeeping.
- `XPLAINFI_BENCH_PROVIDERS` (default `all`; e.g. `xplainfi`) — which algorithms get
  registered/run. `XPLAINFI_BENCH_VERSION` (default installed `packageVersion`) —
  the registry version segment. `reg_path = registries/<lane>/xplainfi-<version>/`.
- Each registry stores `provenance.rds` (xplainfi version + git SHA from `rv.lock`).
  `collect-results.R` stamps every row with `provider` / `xplainfi_version` /
  `xplainfi_sha` and saves `results/<lane>/<provider>-v<version>.rds`.
  **Registries are disposable scratch; the reduced tables are the versioned artifact.**

## Decoupled reruns and cross-version comparison

- Instances are **seed-synchronised across registries** by the batchtools problem
  seed (`problem.seed + repl - 1`, independent of `job.id`): the same
  `(problem, parameters, replication)` yields the identical dataset regardless of
  which algorithms are present. So xplainfi can be re-run alone and still compared
  against frozen reference results.
- Paired-comparison join key: `c(problem, algorithm, learner_type, sampler, feature,
  repl)` + problem params. **Never join on `job.id`** (registry-local).
- Invariant: keep problem defs, design grids, `repls`, and `seed` unchanged across
  versions you compare. Adding problems/params is safe; changing a DGP's generator
  invalidates the join.

## Pre-release check: dev xplainfi vs released (correctness + speed)

1. Point `rproject.toml` xplainfi at the dev branch/SHA, `rv sync`, confirm
   `packageVersion("xplainfi")`. Leave reference deps untouched.
2. Re-run xplainfi only — `XPLAINFI_BENCH_PROVIDERS=xplainfi` → `setup-batchtools.R`
   → `run-experiment.R` (or `submit.R`), for the `importance` (correctness) and
   `runtime` (speed) lanes. Reference impls are not recomputed.
3. `collect-results.R` → writes `xplainfi-v<newversion>.rds`, auto-combines with the
   frozen `reference-v<oldversion>.rds` via `latest_reduced()`.
4. Compare old vs new: `load_reduced(lane, "xplainfi", <old|new>)`, `merge` on the
   paired key. Correctness is **statistical** (sign agreement, CI overlap, importance
   rank correlation) — bitwise equality only for deterministic methods (LOCO), since
   changed code consumes internal RNG differently. Speed = `runtime_new / runtime_old`
   per `(algorithm, n_samples, n_features, ...)`.
5. If good: release to CRAN, pin `rproject.toml` to the tag, commit `rv.lock` + the
   reduced tables so the comparison is reproducible.
- If the old run predates provenance (rows stamp `vNA`), regenerate from the on-disk
  registry (provenance is read per-registry, so version is correct regardless of what
  is installed):
  `XPLAINFI_BENCH_VERSION=<old> Rscript -e 'source("importance/config.R"); source("R/provenance.R"); reg <- batchtools::loadRegistry(conf$reg_path, work.dir=here::here()); save_reduced(reduce_importances(reg, conf$reg_path), "importance", "xplainfi", "<old>")'`

## Gotchas

- `docs/` is gitignored (pkgdown default) but `docs/overview.qmd` is tracked (moved
  there). New files under `docs/` are silently ignored — `git add -f` them.
- Kernel SAGE: xplainfi now ships its own `estimator = "kernel"`; the benchmark's
  `MarginalSAGE_sage` is the *external* `sage` package reference, not xplainfi's.
