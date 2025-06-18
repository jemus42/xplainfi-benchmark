# Script to collect and analyze results
library(batchtools)
library(data.table)
library(ggplot2)

# Load registry
reg_path <- here::here("benchmark", "registry")
reg <- loadRegistry(reg_path, writeable = FALSE)

# Check which jobs are done
(status <- getStatus())

if (status$done == 0) {
  stop("No jobs completed yet. Run the experiment first.")
}

# Get results
cat("\nCollecting results...\n")
results <- reduceResultsDataTable()

# Parse task info from results
results[,
  c("task_type", "n_features", "n_samples") := {
    task_info <- result[[1]]$task_info
    .(task_info$task_type, task_info$n_features, task_info$n_samples)
  },
  by = seq_len(nrow(results))
]

# Extract runtime and method info
results[, runtime := sapply(result, function(x) x$runtime)]
results[, method := sapply(result, function(x) x$method)]
results[, n_permutations := sapply(result, function(x) x$n_permutations %||% NA)]

# Summary statistics
summary_stats <- results[,
  .(
    mean_runtime = mean(runtime, na.rm = TRUE),
    median_runtime = median(runtime, na.rm = TRUE),
    sd_runtime = sd(runtime, na.rm = TRUE),
    n_jobs = .N
  ),
  by = .(method, task_type, n_features, n_samples, n_permutations)
]

cat("\nSummary Statistics:\n")
print(summary_stats)

# Create visualizations
cat("\nCreating plots...\n")

# Runtime by method and problem
p1 <- ggplot(
  summary_stats,
  aes(x = n_features, y = median_runtime, color = method, group = n_features)
) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Runtime Comparison by Problem Type",
    x = "Number of Features",
    y = "Median Runtime (seconds, log scale)",
    color = "Method"
  ) +
  theme_minimal()

ggsave(here::here("benchmark", "runtime_comparison.png"), p1, width = 12, height = 8)

# Runtime by sample size
p2 <- ggplot(
  summary_stats,
  aes(x = n_samples, y = median_runtime, color = method, fill = method, group = method)
) +
  # geom_point(size = 2) +
  geom_boxplot(alpha = 2 / 3) +
  facet_wrap(~task_type, scales = "free") +
  scale_y_log10() +
  scale_fill_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
  labs(
    title = "Runtime vs Sample Size",
    x = "Number of Samples",
    y = "Median Runtime (seconds, log scale)",
    color = "Method",
    fill = "Method"
  ) +
  theme_minimal()

ggsave(here::here("benchmark", "runtime_vs_samples.png"), p2, width = 12, height = 8)

# Method comparison table
method_comparison <- summary_stats[task_type == "friedman1" & n_features == 10 & n_samples == 500]
setorder(method_comparison, median_runtime)

cat("\nMethod Ranking (Friedman1, 10 features, 500 samples):\n")
print(method_comparison[, .(method, median_runtime, sd_runtime)])

# Save detailed results
fwrite(results, here::here("benchmark", "detailed_results.csv"))
fwrite(summary_stats, here::here("benchmark", "summary_results.csv"))
