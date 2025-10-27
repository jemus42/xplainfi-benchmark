# Feature Importance Methods Benchmark

This directory contains a comprehensive benchmark setup using `batchtools` to compare all feature importance methods in the `xplainfi` package.

## Structure

- `config.R` - Configuration settings and experiment parameters
- `helpers.R` - Helper functions for creating learners, measures, and resampling
- `problems.R` - Problem definitions (task generators)
- `algorithms.R` - Algorithm definitions (feature importance methods)
- `experiment.R` - Main experiment setup script
- `run_experiment.R` - Script to execute the benchmark
- `collect_results.R` - Script to collect and analyze results

## Problems

1. **Friedman1**: Classic Friedman regression (fixed 10 features)
2. **Peak**: Peak regression task with controllable dimensions (5, 10, 15, 20 features)
3. **Bike Sharing**: Real-world bike sharing dataset (fixed dimensions)
4. **Correlated**: Correlated features DGP from xplainfi (4 features, configurable correlation)
5. **Ewald**: DGP from Ewald et al. (2024) with correlations and interactions (5 features)
6. **Interactions**: Pure interaction effects DGP from xplainfi (5 features)

## Algorithms

### xplainfi Methods
1. **PFI** - Permutation Feature Importance (marginal sampling)
2. **CFI** - Conditional Feature Importance (supports ConditionalARFSampler, ConditionalGaussianSampler, ConditionalKNNSampler)
3. **RFI** - Relative Feature Importance (conditional sampling with conditioning set)
4. **LOCO** - Leave-One-Covariate-Out
5. **MarginalSAGE** - SAGE with marginal sampling
6. **ConditionalSAGE** - SAGE with conditional sampling (supports multiple samplers)

### Reference Implementations
7. **PFI_mlr3filters** - PFI from mlr3filters package
8. **PFI_iml** - PFI from iml package
9. **PFI_vip** - PFI from vip package

## Usage

1. **Setup the experiment**:
   ```r
   source("benchmark/experiment.R")
   ```

2. **Run the benchmark**:
   ```r
   source("benchmark/run_experiment.R")
   ```

3. **Collect results**:
   ```r
   source("benchmark/collect_results.R")
   ```

## Configuration

The benchmark is configured via `config.R` with the following default settings:

**Sample sizes**: Configurable via `exp_settings$n_samples`
**Feature dimensions**: Configurable via `exp_settings$n_features` (for peak problem)
**Learner types**: featureless, linear, ranger
**n_repeats**: Number of iterations for PFI, CFI, RFI, LOCO
**n_permutations**: Number of permutations for SAGE methods
**sage_n_samples**: Reference dataset size for SAGE methods (currently fixed at 200)
**Samplers**: ConditionalARFSampler, ConditionalGaussianSampler, ConditionalKNNSampler
**Replications**: Number of independent runs per configuration

## Experiment Design

The benchmark creates a comprehensive factorial design combining:
- **6 problems** × **9 algorithms** × **multiple parameter combinations** × **replications**
- Fixed `n_trees = 500` for ranger learner (not varied across experiments)
- Sampler variations for CFI, RFI, and ConditionalSAGE
- Reference implementations (mlr3filters, iml, vip) for validation

**Job tags** for selective analysis:
- `runtime_benchmark`: Featureless learner on peak problem
- `correlation_study`: All correlated DGP experiments
- `dgp_comparison`: xplainfi DGP experiments (correlated, ewald, interactions)

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

## Notes

- Uses interactive cluster functions for local execution
- Handles missing dependencies gracefully (requires `arf`, `partykit`, `mvtnorm` for conditional samplers)
- Each job runs with isolated random seeds for reproducibility# xplainfi-benchmark
