# Script to collect and analyze results
library(batchtools)
library(data.table)
library(ggplot2)
library(dplyr)
library(kableExtra)

# Load registry
source("config.R")
reg <- loadRegistry(reg_path, writeable = FALSE, work.dir = here::here())

# Check which jobs are done
(status <- getStatus())

if (status$done == 0) {
	stop("No jobs completed yet. Run the experiment first.")
}

# Get results
cat("\nCollecting results...\n")
results <- reduceResultsDataTable()


lapply(results$result, \(x) data.table(x$importance[[1]]))


tmpres = data.table::rbindlist(results$result, fill = TRUE)
tmpres = cbind(results[, .(job.id)], tmpres)

res = ijoin(
	tmpres,
	unwrap(getJobPars())[, .(
		job.id,
		algorithm,
		n_repeats,
		sampler,
		n_permutations
	)]
)


# Summary statistics
summary_stats <- res[,
	.(
		mean_runtime = mean(runtime, na.rm = TRUE),
		median_runtime = median(runtime, na.rm = TRUE),
		sd_runtime = sd(runtime, na.rm = TRUE),
		n_jobs = .N
	),
	by = .(
		algorithm,
		task_type,
		learner_type,
		n_features,
		n_samples,
		n_permutations
	)
]

summary_stats


# Runtime by method and problem
(p1 <- ggplot(
	summary_stats,
	aes(
		x = reorder(algorithm, median_runtime),
		y = median_runtime,
		fill = algorithm
	)
) +
	facet_wrap(vars(n_features), ncol = 1, labeller = label_both) +
	coord_flip() +
	geom_boxplot() +
	scale_y_log10() +
	scale_fill_brewer(palette = "Dark2", guide = "none") +
	labs(
		title = "Runtime Comparison by Problem Type",
		x = "Number of Features",
		y = "Median Runtime (seconds, log scale)",
		color = "Method"
	) +
	theme_minimal())

p1

# ggsave(here::here("benchmark", "runtime_comparison.png"), p1, width = 12, height = 8)

# Featureless only
res_fless <- res[learner_type == "featureless" & task_name == "peak"]

res_fless[, .N, by = n_features]
res_fless[, .N, by = n_samples]

res_fless |>
	mutate(
		algorithm = factor(
			algorithm,
			levels = c(
				"PFI",
				"PFI_mlr3filters",
				"CFI",
				"RFI",
				"MarginalSAGE",
				"ConditionalSAGE",
				"LOCO"
			)
		)
	) |>
	ggplot(aes(x = factor(n_features), y = runtime, fill = factor(n_samples))) +
	facet_wrap(vars(algorithm)) +
	geom_boxplot() +
	coord_flip() +
	scale_y_log10() +
	labs(
		title = "Runtime using featurless learner",
		subtitle = "Task: mlbench.peak",
		x = "# Features",
		y = "Runtime (log10 seconds)",
		fill = "# Samples"
	) +
	theme_minimal(base_size = 14) +
	theme(legend.position = "top")


res_fless |>
	filter(grepl("PFI", algorithm)) |>
	ggplot(aes(x = factor(n_features), y = runtime, fill = factor(algorithm))) +
	geom_boxplot() +
	coord_flip() +
	scale_y_log10() +
	labs(
		title = "Runtime using featurless learner",
		subtitle = "Task: mlbench.peak",
		x = "# Features",
		y = "Runtime (log10 seconds)",
		fill = "# Samples"
	) +
	theme_minimal(base_size = 14) +
	theme(legend.position = "top")


# fless_tab <- res_fless |>
#   group_by(algorithm, n_features, n_samples, max_reference_size, n_refits, n_permutations) |>
#   summarize(
#     mean = mean(runtime)
#   ) |>
#   tidyr::pivot_wider(names_from = c("algorithm"), values_from = c("mean")) |>
#   filter(!is.na(n_refits))

fless_tab_sage <- res_fless |>
	filter(algorithm %in% c("MarginalSAGE", "ConditionalSAGE")) |>
	select(-n_refits) |>
	group_by(
		algorithm,
		n_features,
		n_samples,
		max_reference_size,
		n_permutations
	) |>
	summarize(
		mean = mean(runtime),
		median = median(runtime),
		min = min(runtime),
		max = max(runtime),
		sd = sd(runtime)
	) |>
	mutate(across(mean:sd, \(x) round(x, 2))) |>
	mutate(
		meansd = glue::glue("{mean} ({sd})"),
		medianminmax = glue::glue("{median} ({min} - {max})")
	) |>
	tidyr::pivot_wider(
		id_cols = c(
			"n_features",
			"n_samples",
			"max_reference_size",
			"n_permutations"
		),
		names_from = c("algorithm"),
		values_from = c("meansd", "medianminmax")
	) |>
	ungroup()

fless_tab_sage |>
	select(
		n_features,
		n_samples,
		max_reference_size,
		n_permutations,
		meansd_MarginalSAGE
	) |>
	kbl(booktabs = TRUE) |>
	kable_styling() |>
	collapse_rows(columns = c("n_features"))
# pack_rows(index = table(fless_tab_sage$n_features))
