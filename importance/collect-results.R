# Script to collect and analyze results
library(batchtools)
library(data.table)
library(ggplot2)
library(dplyr)
library(kableExtra)

# Load registry
source(here::here("importance", "config.R"))
source(here::here("setup-common.R")) # pkg check + all R/ helpers via source_r()
reg <- loadRegistry(conf$reg_path, writeable = FALSE, work.dir = here::here())
tab <- unwrap(getJobTable())

# Check which jobs are done
(status <- getStatus())

if (status$done == 0) {
	stop("No jobs completed yet. Run the experiment first.")
}

# ============================================================================
# Provenance-stamped reduced table (the durable, versioned artifact)
# ============================================================================
# Registries are disposable scratch; the reduced tables below are what we keep
# and version. Each row carries provider + xplainfi_version + xplainfi_sha, so
# tables from different xplainfi versions can be stacked and still told apart.

collected <- reduce_importances(reg, conf$reg_path)

# Save one file per provider present, keyed by xplainfi version, under
# results/importance/. A frozen reference table produced by an earlier "all" or
# "reference"-only run is retained and reused (see combine below).
for (prov_name in unique(collected$provider)) {
	save_reduced(collected[provider == prov_name], lane = "importance")
}

# Compare a fresh xplainfi run against the frozen reference results without
# re-running the (slow, unchanged) reference implementations. Alignment is safe
# because instances are synchronised across registries by the batchtools problem
# seed (problem.seed + repl - 1), independent of job.id.
xplainfi_now <- collected[provider == "xplainfi"]
reference_frozen <- latest_reduced("importance", "reference")
combined <- combine_reduced(xplainfi_now, reference_frozen)

if (!is.null(reference_frozen)) {
	cli::cli_alert_info(
		"Combined {nrow(xplainfi_now)} fresh xplainfi rows with {nrow(reference_frozen)} frozen reference rows"
	)
}

# Get results
cat("\nCollecting results...\n")
results <- reduceResultsDataTable()


# lapply(results$result, \(x) data.table(x$importance[[1]]))

tmpres <- data.table::rbindlist(results$result, fill = TRUE)
tmpres[, learner_type := NULL]
tmpres[, task_name := NULL]
tmpres <- cbind(results[, .(job.id)], tmpres)

res <- ijoin(
	tmpres,
	unwrap(getJobPars())[, .(
		job.id,
		problem,
		algorithm,
		learner_type,
		# n_samples,
		# n_features,
		correlation,
		n_repeats,
		sampler,
		n_permutations,
		sage_n_samples
	)]
)


# Extract importances
importances <- rbindlist(
	lapply(results$job.id, \(x) {
		importances <- results[job.id == x, result[[1]]$importance]
		importances[, job.id := x]
	}),
	fill = TRUE
)

importances <- merge(res[, -"importance"], importances, by = "job.id")

importances |>
	dplyr::filter(problem == "correlated") |>
	ggplot(aes(x = importance, y = feature, fill = algorithm)) +
	geom_boxplot()

# Runtime
summary_stats <- res[,
	.(
		mean_runtime = mean(runtime, na.rm = TRUE),
		median_runtime = median(runtime, na.rm = TRUE),
		sd_runtime = sd(runtime, na.rm = TRUE),
		n_jobs = .N
	),
	by = .(
		algorithm,
		problem,
		task_type,
		learner_type,
		n_features,
		n_samples,
		n_permutations,
		sampler,
		sage_n_samples
	)
]

summary_stats


# Runtime by method and problem
(p1 <- ggplot(
	summary_stats[sampler == "gaussian"],
	aes(
		x = reorder(algorithm, median_runtime),
		y = median_runtime,
		fill = algorithm
	)
) +
	facet_wrap(vars(problem), ncol = 2, labeller = label_both) +
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
	# mutate(
	# 	algorithm = factor(
	# 		algorithm,
	# 		levels = c(
	# 			"PFI",
	# 			"CFI",
	# 			"MarginalSAGE",
	# 			"ConditionalSAGE",
	# 			"LOCO"
	# 		)
	# 	)
	# ) |>
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
	filter(stringr::str_detect(algorithm, "SAGE")) |>
	group_by(
		algorithm,
		n_features,
		n_samples,
		sage_n_samples,
		n_permutations
	) |>
	summarize(
		n = n(),
		mean = mean(runtime),
		median = median(runtime),
		min = min(runtime),
		max = max(runtime),
		sd = sd(runtime, na.rm = TRUE)
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
			"sage_n_samples",
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
		sage_n_samples,
		n_permutations
	) |>
	kbl(booktabs = TRUE) |>
	kable_styling() |>
	collapse_rows(columns = c("n_features"))
# pack_rows(index = table(fless_tab_sage$n_features))

# New runtime plots per problem ----------

res |>
	dplyr::filter(
		problem == "peak",
		learner_type == "linear",
		is.na(sampler) | sampler == "gaussian",
		n_repeats == 1
	) |>
	dplyr::mutate(
		method = ifelse(!is.na(sampler), glue::glue("{algorithm} ({sampler})"), algorithm)
	) |>
	ggplot(aes(y = reorder(algorithm, runtime), x = runtime / 60)) +
	facet_wrap(vars(n_samples, n_features), labeller = label_both) +
	geom_boxplot() +
	scale_x_log10() +
	labs(
		title = "mlbench.peak with varying n, p",
		subtitle = "Linear model, Gaussian sampler, 1 run",
		x = "Runtime (mins)",
		y = "Algorithm"
	) +
	theme_bw(base_size = 14)


res |>
	dplyr::filter(
		problem == "peak",
		learner_type == "rf"
		# is.na(sampler) | sampler == "arf",
	) |>
	dplyr::mutate(
		method = ifelse(!is.na(sampler), glue::glue("{algorithm} ({sampler})"), algorithm)
	) |>
	ggplot(aes(y = reorder(method, runtime), x = runtime / 60 / 60)) +
	facet_wrap(vars(n_samples, n_features), labeller = label_both) +
	geom_boxplot() +
	scale_x_log10() +
	labs(
		title = "mlbench.peak with varying n, p",
		subtitle = "RF, 1 and 10 repeats",
		x = "Runtime (log 10 hours)",
		y = "Algorithm"
	) +
	theme_bw(base_size = 14)

res |>
	dplyr::filter(problem == "bike_sharing") |>
	dplyr::mutate(
		method = ifelse(!is.na(sampler), glue::glue("{algorithm} ({sampler})"), algorithm)
	) |>
	ggplot(aes(y = reorder(method, runtime), x = runtime / 60 / 60)) +
	facet_wrap(vars(learner_type)) +
	geom_boxplot() +
	scale_x_log10() +
	labs(title = "Runtime per algorithm, learner", x = "Runtime (log10 hours)", y = NULL) +
	theme_bw(base_size = 14) +
	theme(plot.title.position = "plot")


res |>
	dplyr::filter(problem == "bike_sharing") |>
	dplyr::mutate(
		method = ifelse(!is.na(sampler), glue::glue("{algorithm} ({sampler})"), algorithm)
	) |>
	ggplot(aes(x = runtime / 60 / 60, fill = learner_type)) +
	facet_wrap(vars(method)) +
	geom_density(alpha = 1 / 3) +
	scale_x_log10() +
	labs(
		title = "Runtime per algorithm, learner",
		subtitle = "bike_sharing",
		x = "Runtime (log10 hours)",
		y = NULL
	) +
	theme_bw(base_size = 14) +
	theme(legend.position = "bottom")


res |>
	mutate(
		problem = ifelse(
			problem == "correlated",
			glue::glue("{problem} r={correlation}"),
			problem
		),
		problem = glue::glue("{problem} ({n_samples}⨉{n_features})"),
		method = case_when(
			stringr::str_detect(algorithm, "^(Marginal|Conditional)") ~ glue::glue(
				"{algorithm} ({sampler}, {n_permutations} perms)",
				.na = ""
			),
			stringr::str_detect(algorithm, "^(PFI|CFI|LOCO)") ~ glue::glue(
				"{algorithm} ({n_repeats} iter)",
				.na = ""
			)
		),
		minutes = runtime / 60
	) |>
	group_by(method, problem, learner_type) |>
	summarize(
		q25 = quantile(minutes, probs = 0.25),
		median = median(minutes),
		q75 = quantile(minutes, 0.75),
		sd = sd(minutes)
	) |>
	View()


runtime_base <- res |>
	mutate(
		problem = ifelse(
			problem == "correlated",
			glue::glue("{problem} r={correlation}"),
			problem
		),
		problem = glue::glue("{problem} ({n_samples}⨉{n_features})"),
		method = case_when(
			stringr::str_detect(algorithm, "^(Conditional|CFI)") ~ glue::glue(
				"{algorithm} ({sampler})",
				.na = ""
			),
			.default = algorithm
		),
		minutes = runtime / 60
	) |>
	arrange(desc(minutes))


runtime_base_summary <- runtime_base |>
	group_by(algorithm, problem, sampler) |>
	summarize(
		q25 = quantile(minutes, probs = 0.25, na.rm = TRUE),
		median = median(minutes, na.rm = TRUE),
		q75 = quantile(minutes, probs = 0.75, na.rm = TRUE),
		min = min(minutes, na.rm = TRUE),
		max = max(minutes, na.rm = TRUE),
		sd = sd(minutes, na.rm = TRUE)
	) |>
	mutate(across(where(is.numeric), \(x) round(x, 1))) |>
	mutate(
		minutes_fmt = glue::glue("{median} ({q25} - {q75})")
	)
# filter(algorithm == "ConditionalSAGE")

runtime_base_summary |>
	filter(stringr::str_detect(algorithm, "Conditional|CFI")) |>
	mutate(sampler = ifelse(is.na(sampler), "gaussian", sampler)) |>
	tidyr::pivot_wider(
		id_cols = c("problem", "sampler"),
		names_from = "algorithm",
		values_from = "minutes_fmt"
	)

runtime_base_summary |>
	filter(stringr::str_detect(algorithm, "PFI|Marginal|KernelSAGE|LOCO")) |>
	tidyr::pivot_wider(
		id_cols = c("problem"),
		names_from = "algorithm",
		values_from = "minutes_fmt"
	)
