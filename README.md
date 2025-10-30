# Feature Importance Methods Benchmark

This directory contains a comprehensive benchmark setup using `batchtools` to compare all feature importance methods in the `xplainfi` package.

## Structure

- `config.R` - Configuration settings and experiment parameters
- `R/helpers.R` - Helper functions for creating learners, measures, samplers, and resampling
- `R/problems.R` - Problem definitions (task generators)
- `R/algorithms.R` - Algorithm definitions (feature importance methods)
- `setup-batchtools.R` - Main experiment setup script
- `run_experiment.R` - Script to execute the benchmark
- `submit.R` - Script to submit jobs to cluster
- `collect_results.R` - Script to collect and analyze results

## Problems

1. **Friedman1**: Classic Friedman regression (fixed 10 features)
2. **Peak**: Peak regression task with controllable dimensions (5, 10, 50 features)
3. **Bike Sharing**: Real-world bike sharing dataset (fixed dimensions)
4. **Correlated**: Correlated features DGP from xplainfi (4 features, correlation: 0.5, 0.9)
5. **Ewald**: DGP from Ewald et al. (2024) with correlations and interactions (5 features)
6. **Interactions**: Pure interaction effects DGP from xplainfi (5 features)

## Algorithms

### xplainfi Methods
1. **PFI** - Permutation Feature Importance (marginal sampling)
2. **CFI** - Conditional Feature Importance (supports arf, gaussian, knn, ctree samplers)
3. **RFI** - Relative Feature Importance (conditional sampling with conditioning set)
4. **LOCO** - Leave-One-Covariate-Out
5. **MarginalSAGE** - SAGE with marginal sampling
6. **ConditionalSAGE** - SAGE with conditional sampling (supports arf, gaussian, knn, ctree samplers)

### Reference Implementations
7. **PFI_iml** - PFI from iml package (using `compare = "difference"`)
8. **PFI_vip** - PFI from vip package
9. **PFI_fippy** - PFI from fippy Python package (SimpleSampler)
10. **CFI_fippy** - CFI from fippy Python package (Gaussian sampler)
11. **MarginalSAGE_fippy** - Marginal SAGE from fippy Python package
12. **ConditionalSAGE_fippy** - Conditional SAGE from fippy Python package (Gaussian sampler)
13. **KernelSAGE** - Official SAGE implementation with kernel estimator (iancovert/sage)

## Usage

1. **Setup the experiment**:
   ```r
   source("setup-batchtools.R")
   ```

2. **Run the benchmark**:
   ```r
   source("run_experiment.R")
   ```

3. **Collect results**:
   ```r
   source("collect_results.R")
   ```

## Configuration

The benchmark is configured via `config.R` with the following default settings:

- **Sample sizes**: 100, 500, 1000 (via `exp_settings$n_samples`)
- **Feature dimensions**: 5, 10, 50 (via `exp_settings$n_features`, for peak problem)
- **Correlation values**: 0.5, 0.9 (via `exp_settings$correlation`, for correlated problem)
- **Learner types**: featureless, linear, ranger, nnet
- **n_repeats**: 1, 10, 100 (for PFI, CFI, RFI, LOCO)
- **n_permutations**: 5, 10, 30 (for SAGE methods)
- **sage_n_samples**: 200 (marginalization dataset size for SAGE methods)
- **Samplers**: arf, gaussian, knn, ctree (for CFI, RFI, ConditionalSAGE)
- **Replications**: N independent runs per configuration (TBD)
- **Random seed**: 2025

## Experiment Design

The benchmark creates a comprehensive factorial design combining:
- **6 problems** × **13 algorithms** × **4 learner types** × **parameter combinations** × **N replications**
- Problems vary in sample size and feature dimensions (where applicable)
- Each problem is paired with all learner types (featureless, linear, ranger, nnet)
- Fixed `n_trees = 500` for ranger learner (not varied across experiments)
- Sampler variations for CFI, RFI, and ConditionalSAGE
- Reference implementations from iml, vip, fippy (PFI, CFI, Marginal/Conditional SAGE), and sage (KernelSAGE) for validation

**Key design principle**: Learner type is part of the problem design (not algorithm design), ensuring fair comparison across methods with identical models.

**Job tags** for selective analysis:
- `runtime`: Featureless learner on peak problem
- `dgp_comparison`: Synthetic DGPs (friedman1, ewald, interactions, correlated)
- `real_data`: Real-world data (bike_sharing)

## Expected Outputs

- `detailed_results.csv` - Full results for each job
- `summary_results.csv` - Aggregated statistics
- `runtime_comparison.png` - Runtime comparison plots
- `runtime_vs_samples.png` - Runtime vs sample size plots

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

The benchmark requires the following R packages:
- Core: `xplainfi`, `mlr3`, `mlr3learners`, `mlr3pipelines`, `batchtools`, `reticulate`
- Data: `data.table`, `mlbench`, `mlr3data`
- Samplers: `arf`, `partykit`, `mvtnorm`
- Reference implementations: `iml`, `vip`
- Learners: `nnet` (neural networks)
- Utilities: `checkmate`, `digest`, `here`, `cli`, `fs`

Python dependencies (managed via `reticulate::py_require()` with `uv`):
- `scikit-learn` - Machine learning models for Python implementations
- `pandas` - Required for fippy (DataFrames with `.columns`, Series with `.to_numpy()`)
- `fippy` - Python reference implementation for PFI, CFI, Marginal SAGE, and Conditional SAGE
- `sage-importance` - Official SAGE implementation with KernelSAGE estimator

## Notes

- Uses interactive cluster functions for local execution
- Each job runs with isolated random seeds for reproducibility
- Helper functions (`create_learner()`, `create_sampler()`, `create_measure()`, `create_resampling()`) ensure consistent component creation
- Sampler compatibility: Some samplers may not support all data types (e.g., Gaussian sampler doesn't support mixed feature types)
- Python/fippy integration: Categorical features are automatically one-hot encoded for scikit-learn compatibility
