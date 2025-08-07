# Script to collect and analyze results
library(batchtools)
library(data.table)
library(ggplot2)

# Load registry
reg_path <- here::here("registry")
reg <- loadRegistry(reg_path, writeable = FALSE)

# Check which jobs are done
(status <- getStatus())

if (status$done == 0) {
  stop("No jobs completed yet. Run the experiment first.")
}

# Get results
cat("\nCollecting results...\n")
results <- reduceResultsDataTable()


lapply(results$result, \(x) data.table(x$importance))

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
results[, learner_type := sapply(result, function(x) x$learner_type)]

# Summary statistics
summary_stats <- results[,
  .(
    mean_runtime = mean(runtime, na.rm = TRUE),
    median_runtime = median(runtime, na.rm = TRUE),
    sd_runtime = sd(runtime, na.rm = TRUE),
    n_jobs = .N
  ),
  by = .(method, task_type, learner_type, n_features, n_samples, n_permutations)
]

cat("\nSummary Statistics:\n")
print(summary_stats)

# Create visualizations
cat("\nCreating plots...\n")

# Runtime by method and problem
p1 <- ggplot(
  summary_stats,
  aes(x = method, y = median_runtime, fill = method)
) +
  facet_wrap(vars(n_features)) +
  coord_flip() +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Runtime Comparison by Problem Type",
    x = "Number of Features",
    y = "Median Runtime (seconds, log scale)",
    color = "Method"
  ) +
  theme_minimal()

# ggsave(here::here("benchmark", "runtime_comparison.png"), p1, width = 12, height = 8)
