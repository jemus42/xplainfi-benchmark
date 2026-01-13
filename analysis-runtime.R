library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(batchtools)
source(here::here("R", "analysis.R"))

file_results <- fs::path(here::here("results", "runtime"), "results", ext = "rds")
file_runtime <- fs::path(here::here("results", "runtime"), "runtime", ext = "rds")
file_job_pars <- fs::path(here::here("results", "runtime"), "jobs", ext = "rds")

# Aggregate and store results file if not available
if (!fs::file_exists(file_results)) {
	# Loading registry with warnings suppressed which are due to file paths changing between cluster/workstation/etc
	reg <- suppressWarnings(loadRegistry(
		"registries/runtime/xplainfi-0.2.1/",
		writeable = FALSE,
		work.dir = here::here()
	))
	tab <- unwrap(getJobTable())
	saveRDS(tab, file_job_pars)
	# Subset to configurations for paper
	tab <- tab[
		repl <= 25 &
			n_samples < 10000 &
			(sage_n_samples < 200 | n_repeats < 100) &
			(is.na(sampler) | sampler %in% c("gaussian", "knn", "simple")) &
			n_features < 50
	]
	results <- reduceResultsDataTable(ids = findDone(tab))
	fs::dir_create(here::here("results", "runtime"))
	saveRDS(results, file_results)
}


# I should have used targets.
if (!fs::file_exists(file_runtime)) {
	runtimes <- clean_results_runtime(
		results = readRDS(file_results),
		job_pars = readRDS(file_job_pars)
	)
	saveRDS(runtimes, file_runtime)
}

runtimes <- readRDS(file_runtime)


table(runtimes$algorithm)
table(runtimes$sampler)
table(runtimes$n_samples)
table(runtimes$n_features)
table(runtimes$n_repeats)
table(runtimes$sage_n_samples)
table(runtimes$n_permutations)


runtimes |>
	# filter(stringr::str_detect(algorithm, "MarginalSAGE")) |>
	filter(method == "PFI") |>
	# filter(stringr::str_detect(algorithm, "CFI") & sampler == "gaussian") |>
	# mutate(algorithm = glue::glue("{algorithm} {sampler}", .na = "")) |>
	filter(n_features == 20, n_samples == 5000) |>
	ggplot(aes(x = runtime, y = reorder(algorithm, runtime))) +
	facet_wrap(vars(n_samples, n_features, n_repeats), labeller = label_both) +
	geom_boxplot() +
	scale_x_log10() +
	labs(
		title = "PFI runtime",
		subtitle = "Varying by number of permutation iterations",
		x = "Runtime (s)",
		y = "Algorithm"
	) +
	theme_minimal(base_size = 14) +
	theme(plot.title.position = "plot")


runtimes |>
	filter(method == "mSAGE") |>
	# filter(stringr::str_detect(algorithm, "PFI")) |>
	# filter(stringr::str_detect(algorithm, "CFI") & sampler == "gaussian") |>
	# mutate(algorithm = glue::glue("{algorithm} {sampler}", .na = "")) |>
	filter(n_features == 20, n_samples == 5000) |>
	summarise(
		q25_secs = quantile(runtime, prob = 0.25),
		median_secs = median(runtime),
		q75_secs = quantile(runtime, prob = 0.75),
		.by = c("learner_type", "n_features", "n_permutations", "package")
	) |>
	mutate(across(where(is.numeric), \(x) round(x, 2))) |>
	arrange(median_secs)
