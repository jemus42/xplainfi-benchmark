# `xplainfi` Feature Importance Methods Benchmark

A [`batchtools`](https://mllg.github.io/batchtools/) benchmark for the
[`xplainfi`](https://github.com/mlr-org/xplainfi) package. It evaluates xplainfi's
feature-importance methods on two axes — **results** (are the importance values
correct?) and **runtime** (how fast are they?) — and cross-checks them against
independent **reference implementations** (`iml`, `vip`, and the Python `fippy` / `sage`
packages).

## How it works

- **Two lanes.** `importance/` compares importance *values* across methods and
  implementations on data-generating processes with known structure; `runtime/` measures
  *timing* as data size and dimensionality scale. Each lane is a self-contained set of
  scripts sharing the same shape (`config` → `setup-batchtools` → `run-experiment` →
  `collect-results`) and the shared code in `R/`.
- **Two kinds of algorithm.** *xplainfi* methods (the package under test) and *reference*
  implementations (external, for validation). Which is which is derived from the
  algorithm name by convention — reference impls end in `_iml` / `_vip` / `_fippy` /
  `_sage`; everything else is xplainfi.
- **Versioned, decoupled results.** Registries and reduced result tables are namespaced
  by xplainfi version, so results for different versions are retained side by side and
  xplainfi can be re-benchmarked *without* re-running the reference implementations. See
  [Versioning and selective reruns](#versioning-and-selective-reruns).

## Structure

```
.
├── importance/          # Importance benchmark lane
│   ├── config.R         # Experiment parameters
│   ├── setup-batchtools.R
│   ├── run-experiment.R # Group jobs by backend + resource tier, submit
│   ├── collect-results.R
│   ├── analysis.R       # Post-processing and figures
│   ├── eta.R            # Runtime/ETA estimation
│   └── shiny.R          # Interactive results explorer
├── runtime/             # Runtime benchmark lane
│   ├── config.R
│   ├── setup-batchtools.R
│   ├── run-experiment.R
│   ├── analysis.R
│   ├── eta.R
│   └── shiny.R
├── R/                   # Shared functions
│   ├── helpers.R        # Learner, measure, sampler, resampling helpers
│   ├── helpers-python.R # Python/fippy integration helpers
│   ├── problems.R       # Problem definitions (task generators)
│   ├── algorithms.R     # Algorithm definitions (FI methods)
│   ├── submit-helpers.R # Job grouping/chunking for submission
│   └── plotting.R       # Plot saving utilities
├── setup-common.R       # Shared package dependency checks
├── batchtools.conf.R    # Cluster configuration (gitignored)
├── rproject.toml / rv.lock   # R dependencies (managed by `rv`)
├── pyproject.toml / uv.lock  # Python dependencies (managed by `uv`)
├── registries/          # Batchtools registries, namespaced by xplainfi version (scratch)
└── results/             # Versioned reduced result tables (importance/, runtime/)
```

Agent-facing project notes live in `.claude/CLAUDE.md`.

## Problems (DGPs)

The **importance** lane runs data-generating processes with known importance structure:

- **friedman1** — classic Friedman1 regression (`mlbench`), 10 features (5 informative)
- **correlated** — correlated-features DGP, correlation ∈ {0.2, 0.5, 0.7, 0.9}
- **ewald** — DGP from Ewald et al. (2024) with correlations and interactions
- **interactions** — pure interaction-effects DGP
- **independent** / **confounded** / **mediated** — xplainfi `sim_dgp_*` DGPs isolating
  each causal structure
- **bike_sharing** — real-world data (factors converted to numeric for a fair comparison)

The **runtime** lane runs a single scalable task, **peak** (`mlbench`), varying feature
count and sample size to profile how methods scale.

## Algorithms

### xplainfi methods (package under test)
- **PFI** — Permutation Feature Importance
- **CFI** — Conditional FI (samplers: arf, gaussian, knn, ctree)
- **LOCO** — Leave-One-Covariate-Out
- **MarginalSAGE** / **ConditionalSAGE** — SAGE with marginal / conditional sampling
- **RFI** — Relative FI (implemented but currently disabled in the design)

### Reference implementations (validation)
- **PFI_iml** — PFI from the `iml` package (`compare = "difference"`)
- **PFI_vip** — PFI from the `vip` package
- **PFI_fippy** / **CFI_fippy** — PFI / CFI from the Python `fippy` package
- **MarginalSAGE_fippy** / **ConditionalSAGE_fippy** — SAGE from `fippy`
- **MarginalSAGE_sage** — kernel-estimator SAGE from the Python `sage` package
  (`sage-importance`, aka iancovert/sage)

> Note: xplainfi now ships its own kernel SAGE (`estimator = "kernel"`); the
> `MarginalSAGE_sage` algorithm above is the *external* `sage` package, kept as a
> reference for that estimator.

## Usage

Each lane follows the same workflow. For the importance benchmark:

1. **Setup the experiment**:
   ```r
   source("importance/setup-batchtools.R")
   ```

2. **Run the benchmark**:
   ```r
   source("importance/run-experiment.R")
   ```

3. **Collect results**:
   ```r
   source("importance/collect-results.R")
   ```

Replace `importance/` with `runtime/` for the runtime benchmark.

## Versioning and selective reruns

Results are namespaced and version-stamped so that results for different
xplainfi versions are retained side by side, and so the package under test can be
re-benchmarked without re-running the (slow, rarely-changing) reference
implementations.

Two environment variables control this:

- **`XPLAINFI_BENCH_PROVIDERS`** — which implementations to include. Comma-separated
  subset of `xplainfi`, `reference`, or `all` (default). Provider is derived from
  the algorithm name by convention: reference implementations end in `_iml` /
  `_vip` / `_fippy` / `_sage`; everything else is xplainfi. Adding a new method
  (e.g. a kernel SAGE estimator) therefore needs no extra bookkeeping.
- **`XPLAINFI_BENCH_VERSION`** — override the version segment of the registry path.
  Defaults to the installed `packageVersion("xplainfi")`. Set it to collect or
  inspect a historical registry.

The registry lives at `registries/<lane>/xplainfi-<version>/`, and a
`provenance.rds` (xplainfi version + resolved git SHA from `rv.lock`) is written
into it. `collect-results.R` stamps every result row with `provider`,
`xplainfi_version`, and `xplainfi_sha`, then saves a durable reduced table per
provider under `results/<lane>/<provider>-v<version>.rds`. **Treat registries as
disposable scratch; the reduced tables are the versioned artifact.**

Typical workflow after an xplainfi version bump — re-run only xplainfi and
compare against the frozen reference results:

```sh
# One-time (or when the reference stack changes): full run, all providers
Rscript -e 'source("importance/setup-batchtools.R")'   # then run + collect

# After bumping xplainfi: re-run xplainfi only, reuse frozen reference results
XPLAINFI_BENCH_PROVIDERS=xplainfi Rscript -e 'source("importance/setup-batchtools.R")'
# run-experiment.R, then collect-results.R combines fresh xplainfi + frozen reference
```

Instances line up across separate registries because batchtools seeds each
problem instance with `problem.seed + repl - 1` (independent of `job.id`), so the
same `(problem, parameters, replication)` yields the identical dataset regardless
of which algorithms are present. For **paired** comparisons join on the semantic
key `(problem, learner_type, sampler, feature, repl, <problem params>)`, never on
`job.id` (which is registry-local). This holds only while problem definitions and
their design grids are unchanged — adding problems/parameters is safe; changing a
problem's generator invalidates comparison against older tables.

## Configuration

Each lane has its own `config.R`. The **importance** lane (`importance/config.R`)
defaults:

- **Sample size**: 5000 (`conf$n_samples`)
- **Correlation** (correlated DGP): 0.2, 0.5, 0.7, 0.9
- **Learner types**: linear, rf, mlp, boosting
- **n_repeats** (PFI/CFI/LOCO): 100
- **n_permutations** (SAGE): 100, 200 — with early stopping (`min_permutations = 20`)
- **sage_n_samples** (Monte-Carlo background size): 100
- **Samplers** (CFI/ConditionalSAGE): arf, gaussian, knn
- **Replications**: 50
- **Random seed**: 2025

The **runtime** lane (`runtime/config.R`) instead sweeps *scale*: `n_samples` ∈
{100, 250, 1000, 5000, 10000} × `n_features` ∈ {5, 10, 25, 50}, with `learner_types`
{featureless, linear}, `n_repeats` {1, 50, 100}, more `n_permutations`/`sage_n_samples`
grid points, and SAGE early stopping off.

## Experiment Design

The importance benchmark is a factorial design over **8 problems × ~12 algorithms ×
learner types × parameter grids × replications**, with:

- learner type part of the *problem* design (not the algorithm), so every method is
  compared on identically fitted models;
- fixed `n_trees = 500` for the rf learner;
- sampler variations for CFI and ConditionalSAGE;
- reference implementations (iml, vip, fippy, sage) run alongside for validation.

**Key design principle**: learner type is part of the problem design, ensuring a fair
comparison across methods using identical models.

**Job tags** for selective analysis (provider tags are assigned by convention):
- `xplainfi`: methods from the package under test
- `reference`: external comparison implementations (iml, vip, fippy, sage)
- `python`: reticulate-backed reference implementations (fippy, sage)
- `real_data`: real-world data (bike_sharing)

## Expected Outputs

`collect-results.R` reduces a registry into a long, provenance-stamped table (one row
per feature per job, carrying `provider` / `xplainfi_version` / `xplainfi_sha`) and saves
it under `results/<lane>/<provider>-v<version>.rds`. These reduced tables are the durable,
comparable artifact; `analysis.R` and `shiny.R` consume them to produce figures and the
interactive explorer.

## Reproducibility

The benchmark ensures reproducibility through a carefully designed resampling strategy:

- **Within-replication consistency**: All methods compared within a single replication use **identical** train/test splits for fair comparison
- **Across-replication variation**: Different replications use **different** splits to ensure independent runs
- **Implementation**: The `instantiate_resampling()` helper uses `digest::digest2int(task$hash) + replication` to generate deterministic but varying seeds

This is achieved by:
1. Each problem "owns" its learner and instantiated resampling
2. The `instantiate_resampling(resampling, task, job$repl)` function generates a seed from:
   - Task-specific hash (ensures consistency for same task)
   - Replication number (ensures variation across replications)
3. All algorithms within a problem-replication use the same resampling object

## Package Dependencies

R dependencies are managed with [`rv`](https://github.com/A2-ai/rv) (`rproject.toml` +
`rv.lock`); run `rv sync` to install. Key packages:
- Core: `xplainfi`, `mlr3`, `mlr3learners`, `mlr3pipelines`, `mlr3fselect`, `batchtools`, `reticulate`
- Data: `data.table`, `mlbench`, `mlr3data`
- Samplers: `arf`, `partykit`, `mvtnorm`
- Reference implementations: `iml`, `vip` (archived on CRAN, installed from [r-universe](https://bgreenwell.r-universe.dev) — see [bgreenwell/vip](https://github.com/bgreenwell/vip); needs rv >= 0.22.2, older rv panics on r-universe repos)
- Learners: `ranger` (rf), `mlr3torch` (mlp), `xgboost` (boosting), base `stats::lm` (linear)
- Utilities: `checkmate`, `digest`, `here`, `cli`, `fs`

### Python Environment Setup

Python dependencies are managed via `uv` with a local `.venv` for full reproducibility.

**Requirements:**
- `uv` (install via `brew install uv` or see [uv documentation](https://docs.astral.sh/uv/getting-started/installation/))
- Python 3.12 (uv should manage this automatically, see also `.Rprofile` to nudge reticulate in the right direction)

**Setup:**

```bash
# Create and sync the Python environment with all dependencies
uv sync
```

This creates a `.venv` directory with exact package versions locked in `uv.lock`.

**Python packages** (see `pyproject.toml` for specifications):
- `numpy>=1.26.0` - Compatible with Python 3.12+
- `pandas>=2.1.0` - Compatible with Python 3.12+
- `scikit-learn>=1.3.0` - Machine learning models for Python implementations
- `category-encoders>=2.6.0` - Categorical feature encoding for sklearn pipelines
- `xgboost>=2.0.0` - Gradient boosting
- `torch>=2.7.1` - CPU-only PyTorch (required by fippy)
- `fippy` (commit `a7a37aa`) - Python reference implementation for PFI, CFI, Marginal SAGE, and Conditional SAGE
- `sage-importance>=0.0.4` - Official SAGE implementation with kernel estimator (`MarginalSAGE_sage`)

The `uv.lock` file ensures exact reproducibility across machines. To update dependencies after modifying `pyproject.toml`:

```bash
uv lock
uv sync
```

**Troubleshooting: External Python Environments**

For reproducibility, this project forces R/reticulate to use the local `.venv` Python environment (configured in `.Rprofile`). However, external tools like **spack** (common on HPC systems) can interfere by setting environment variables like `PYTHONPATH` that point to incompatible Python packages.

The `.Rprofile` includes safeguards to prevent this:
- `RETICULATE_PYTHON` is set to explicitly use `.venv/bin/python`
- `PYTHONPATH` is unset to prevent external packages from being found

If you encounter numpy import errors like "you should not try to import numpy from its source directory", check for conflicting environment variables:

```bash
env | grep -i python
```

If `PYTHONPATH` points to packages for a different Python version (e.g., spack's Python 3.14 while the venv uses Python 3.11), that's the cause. The `.Rprofile` should handle this automatically, but you may need to restart R after making changes.

## Notes

- Uses interactive cluster functions for local execution
- Each job runs with isolated random seeds for reproducibility
- Helper functions (`create_learner()`, `create_sampler()`, `create_measure()`, `create_resampling()`) ensure consistent component creation
- Sampler compatibility: Some samplers may not support all data types (e.g., Gaussian sampler doesn't support mixed feature types)
- Python/fippy integration: Categorical features are automatically one-hot encoded for scikit-learn compatibility
- Featureless learner is only used for xplainfi runtime benchmarking; jobs pairing it with reference implementations (e.g. `MarginalSAGE_sage`, whose convergence detection would otherwise run indefinitely) are removed automatically during setup.
