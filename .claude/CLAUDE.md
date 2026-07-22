# xplainfi-benchmark

`batchtools` benchmark comparing **xplainfi**'s feature-importance methods against
reference implementations, on two axes: **results** (correctness) and **runtime**.

## Layout

- Two parallel lanes, `importance/` and `runtime/`, each with the same scripts:
  `config.R`, `setup-batchtools.R`, `run-experiment.R`, `collect-results.R`,
  `analysis.R`, `eta.R`, `shiny.R`.
- `R/` shared: `helpers.R`, `helpers-python.R` (fippy/reticulate), `problems.R`
  (DGP generators `prob_*`), `algorithms.R` (`algo_*` fns), `submit-helpers.R`
  (job grouping/chunking, sourced by `run-experiment.R`), `estimate.R`
  (runtime estimation `write_estimates()` + `read_estimates()`, sourced by
  `eta.R` and `run-experiment.R`), `plotting.R`, `provenance.R` (provider +
  versioning plumbing).
- `setup-common.R` is the bootstrap: pkg checks + `source_r()`, which sources every
  `.R` in `R/` into the global env (like `targets::tar_source()`). Scripts source
  `setup-common.R` instead of listing individual `R/` files; a new helper in `R/` is
  picked up automatically. (Registry workers still get an explicit `source=` list.)
- `registries/<lane>/xplainfi-<version>/` — batchtools registries (gitignored, scratch).
- `results/<lane>/` — durable reduced tables (tracked).
- Deps via `Makefile`: `make setup` = `rv sync` (R) + `uv sync --frozen` (Python
  `.venv`, one shared env, not per-job) + `torch::install_torch()` (libtorch for
  mlr3torch). `make check` reports sync status of all three without installing —
  run before submitting. Individual targets: `r-deps`, `py-deps`, `torch`.
- Format R with `air format` (tabs, width 100). Files end with a newline.

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
   → `run-experiment.R`, for the `importance` (correctness) and
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

## Submission (`run-experiment.R` + `R/submit-helpers.R`)

- `plan_submission()` groups pending jobs on two axes: **backend** (R vs Python,
  from the `python` tag) then **resource tier** (runtime buckets → walltime). Each
  group is one `submitJobs()` call.
- **Backend isolation is the point of the split**: batchtools runs a `chunk`'s jobs
  in one R session, so R torch (mlr3torch/libtorch) and Python torch (reticulate)
  must never share a chunk. Chunk ids are offset per group; a collision aborts.
- **QoS is not set** — the Slurm template derives it from `walltime`. Resources
  carry only `walltime` (per tier) + `memory`.
- **Estimates** (`R/estimate.R`): `eta.R` runs `write_estimates()` → `eta-<lane>.rds`
  (runtime only; batchtools memory estimation was dropped — memory comes from the
  external `slurm-memcheck` utility, materialised as `mem-<lane>.rds` if used).
  `run-experiment.R` reads both via `read_estimates()`. Both files are gitignored
  scratch. Missing runtime → chunk by job count; missing memory → `mem_default`.
- **OOM recovery**: sourcing `run-experiment.R` resubmits *everything* outstanding
  (`todo()` = expired + never-run) with expired jobs' memory doubled via
  `escalate_memory()` (reads the last request from the registry, so repeated
  expiries compound — no counter). For an interactive "just retry the OOM'd ones"
  pass, `resubmit_expired()` scopes to expired only, still grouped by backend +
  tier (`resubmit_expired(factor = 4)`, `submit = FALSE` to inspect first). A group
  requests the max memory of its members, so a bumped job lifts its chunk.
- LOCO is pinned to `n_repeats = 1L` (refits, repeats are wasted work).
- Cluster functions come from `batchtools.conf.R` only — never set
  `reg$cluster.functions` in `run-experiment.R`.

## Threading parity

- `default.resources$ncpus = 2` (batchtools.conf.R). `n_threads()` (R/helpers.R)
  wraps `parallelly::availableCores()` — honors the Slurm allocation, cgroup quotas,
  etc. (all node cores off-cluster) — applied uniformly so every implementation gets
  the same CPU budget. Bias the thread count and the runtime comparison is meaningless.
- Explicit thread args set to `n_threads()`: R ranger `num.threads`, xgboost
  `nthread`, torch `torch_set_num_threads` (was uncapped → grabbed the whole node);
  Python sklearn RF / XGBoost `n_jobs`.
- BLAS-bound learners (R `lm`, sklearn linear/MLP, numpy) have no thread arg — they
  follow `OMP_NUM_THREADS`, which the BIPS Slurm job prolog sets cluster-wide, so R
  and Python BLAS already match (nothing to configure here).

## Shared working directory — agents read this first

The project directory is shared: the user runs interactive R sessions and submits
cluster jobs against the same paths an agent sees. Two things follow.

- **Never delete, rebuild, or submit into a registry you did not create.**
  `registries/<lane>/xplainfi-<version>/` is very likely a live pretest with jobs
  queued against it, even when it looks like leftover scratch. An agent once
  `rm -rf`'d one mid-run; the symptom was baffling (`findDone()` listing jobs whose
  `loadResult()` failed, because the user's in-memory registry object outlived the
  directory).
  For any agent-side verification, namespace it:
  `XPLAINFI_BENCH_VERSION=agent-scratch Rscript importance/setup-batchtools.R`
  gives `registries/importance/xplainfi-agent-scratch/`, which cannot collide.
  Delete only that. Ask before touching anything else under `registries/`.

- **`.venv` is shared but machine-specific.** The host and a yolobox resolve
  different interpreters, so each side's `uv sync` rebuilds `.venv` and breaks the
  other. Set `UV_PROJECT_ENVIRONMENT` to a machine-local path (e.g. `.venv-yolobox`)
  rather than running `uv` against the shared one; `.Rprofile` and
  `R/helpers-python.R` both read that variable, so uv and reticulate stay in
  agreement. Never `uv run` here — it re-resolves and rebuilds the environment.

## Gotchas

- `docs/` is gitignored (pkgdown default) but `docs/overview.qmd` is tracked (moved
  there). New files under `docs/` are silently ignored — `git add -f` them.
- Kernel SAGE: xplainfi now ships its own `estimator = "kernel"`; the benchmark's
  `MarginalSAGE_sage` is the *external* `sage` package reference, not xplainfi's.
- Estimate files (`eta-<lane>.rds`, `mem-<lane>.rds`) are keyed by `job.id`, which is
  registry-local. `write_estimates()` stamps the registry it came from and
  `read_estimates(reg_path =)` discards a file that does not match — an unstamped or
  foreign file silently applies one registry's runtimes to another's jobs, which is
  how a submission ended up as 3 chunks of several hundred jobs.
- A chunk is ONE Slurm job running its members sequentially, so chunk size is the
  blast radius of a single OOM: every job in it expires, most as bystanders. A chunk
  log ending in `Job terminated successfully` means it died in the *next* job.
  `expired_overview()` reports this per chunk; `sacct -j <batch.id>` is the authority
  on OOM vs walltime (batchtools' `mem.used` is a gc() R-heap figure and misses
  allocations outside R — the `slurm-memcheck` utility parses sacct instead).
