# Feature Importance Methods Benchmark

This directory contains a comprehensive benchmark setup using `batchtools` to compare all feature importance methods in the `xplainfi` package.

## Structure

- `config.R` - Configuration settings and experiment parameters
- `problems.R` - Problem definitions (task generators)
- `algorithms.R` - Algorithm definitions (feature importance methods)
- `experiment.R` - Main experiment setup script
- `run_experiment.R` - Script to execute the benchmark
- `collect_results.R` - Script to collect and analyze results

## Problems

1. **Friedman1**: Classic Friedman regression (fixed 10 features)
2. **Peak**: Peak regression task with controllable dimensions (5, 10, 15, 20 features)
3. **Bike Sharing**: Real-world bike sharing dataset (fixed dimensions)

## Algorithms

1. **PFI** - Permutation Feature Importance (marginal sampling)
2. **CFI** - Conditional Feature Importance (conditional sampling with ARF)
3. **RFI** - Relative Feature Importance (conditional sampling with conditioning set)
4. **MarginalSAGE** - SAGE with marginal sampling
5. **ConditionalSAGE** - SAGE with conditional sampling (requires arf)
6. **LOCO** - Leave-One-Covariate-Out
7. **LOCI** - Leave-One-Covariate-In

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

The experiment tests:
- Sample sizes: 500, 1000
- Feature dimensions: 5, 10, 15, 20 (for synthetic tasks)
- Permutations: 5, 10 (for permutation-based methods)
- 3 replications per configuration

## Expected Outputs

- `detailed_results.csv` - Full results for each job
- `summary_results.csv` - Aggregated statistics
- `runtime_comparison.png` - Runtime comparison plots
- `runtime_vs_samples.png` - Runtime vs sample size plots

## Notes

- Uses interactive cluster functions for local execution
- Reproducible seeding for all experiments
- Handles missing dependencies gracefully (e.g., CFI/RFI require `arf`)
- Each job runs with isolated random seeds for reproducibility# xplainfi-benchmark
