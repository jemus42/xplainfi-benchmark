library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(batchtools)
source(here::here("R", "analysis.R"))

file_results <- fs::path(here::here("results", "importance"), "results", ext = "rds")
file_importance <- fs::path(here::here("results", "importance"), "importances", ext = "rds")
file_job_pars <- fs::path(here::here("results", "importance"), "jobs", ext = "rds")

# Aggregate and store results file if not available
if (!fs::file_exists(file_results)) {
	# Loading registry with warnings suppressed which are due to file paths changing between cluster/workstation/etc
	reg <- suppressWarnings(loadRegistry(
		"registries/importance/xplainfi-0.2.1/",
		writeable = FALSE,
		work.dir = here::here()
	))
	tab <- unwrap(getJobTable())
	saveRDS(tab, file_job_pars)

	# Subset to configurations for paper
	tab <- tab[
		(is.na(sage_n_samples) | sage_n_samples == 100) &
			(is.na(n_permutations) | n_permutations == 100) &
			(is.na(sampler) | sampler != "arf") &
			repl <= 25
	]
	results <- reduceResultsDataTable(ids = findDone(tab))
	fs::dir_create(here::here("results", "importance"))
	saveRDS(results, file_results)
}

# I should have used targets.
if (!fs::file_exists(file_importance)) {
	importances <- clean_results_importance(
		results = readRDS(file_results),
		job_pars = readRDS(file_job_pars)
	)
	saveRDS(importances, file_importance)
}

importances <- readRDS(file_importance)

# https://coolors.co/1e3888-ef476f-f5e663-ffad69-9c3848
pal_package = c(
	xplainfi = "#1e3888",
	fippy = "#8CD867",
	vip = "#A31621",
	iml = "#ffad69",
	sage = "#EF476F"
)


# SAGE convergence check -------------------------------------------------

importances[
	!is.na(n_permutations_used),
	list(
		min_perms = min(n_permutations_used),
		max_perms = max(n_permutations_used),
		median_perms = median(n_permutations_used),
		mean_perms = mean(n_permutations_used)
	),
	by = .(method, problem_clean, package)
][, fmt := sprintf("%.01f (<= %.01f)", mean_perms, max_perms)][] |>
	dcast(problem_clean + method ~ package)

importances[!is.na(n_permutations_used)] |>
	ggplot(
		aes(x = n_permutations_used, y = problem_clean, color = algorithm_lab, fill = algorithm_lab)
	) +
	geom_boxplot() +
	scale_fill_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
	theme_minimal(base_size = 16) +
	theme(legend.position = "top")

importances[!is.na(n_permutations_used)] |>
	ggplot(
		aes(x = n_permutations_used, y = runtime / 60, color = algorithm_lab, fill = algorithm_lab)
	) +
	facet_wrap(vars(problem_clean)) +
	geom_point(size = 2, shape = 21) +
	geom_smooth() +
	scale_y_log10() +
	scale_fill_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
	labs(y = "runtime (minutes)", x = "permutations used") +
	theme_minimal(base_size = 16) +
	theme(legend.position = "top")


# Learner performance ----------------------------------------------------

importances[,
	# We have 1 row per feature and performance is duplicated, so we unique() it here
	.(learner_performance = round(100 * mean(learner_performance), 2)),
	by = .(repl, method, language, problem_clean, learner_type)
][] |>
	# dplyr::select(-package) |>
	dcast(
		repl + method + problem_clean + learner_type ~ language,
		# fun.aggregate = \(x) {
		# 	list(unique(x))
		# },
		value.var = "learner_performance"
	) |>
	ggplot(aes(x = R, y = Python, color = learner_type, fill = learner_type)) +
	# facet_wrap(vars(problem)) +
	geom_point(size = 3, shape = 21, stroke = .1, color = "black") +
	geom_abline() +
	scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
	theme_minimal(base_size = 16) +
	theme(legend.position = "top")


importances |>
	ggplot(aes(x = learner_type, y = learner_performance, color = language, fill = language)) +
	coord_flip() +
	facet_wrap(vars(problem_clean)) +
	geom_boxplot() +
	scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill")) +
	theme_minimal(base_size = 16) +
	theme(legend.position = "top")

# Plots --------------------------------------------------------------
plot_importance(importances[correlation == 0.25], problem = "correlated", method = "PFI")
plot_importance(importances[correlation == 0.75], problem = "correlated", method = "PFI")

plot_importance(importances[correlation == 0.25], problem = "correlated", method = "CFI")
plot_importance(importances[correlation == 0.75], problem = "correlated", method = "CFI")

plot_importance(importances, type = "raw", problem = "correlated", method = "PFI")
plot_importance(importances, type = "scaled", problem = "correlated", method = "PFI")
plot_importance(importances, type = "rank", problem = "correlated", method = "PFI")

plot_importance(importances, problem = "correlated", method = "CFI")
plot_importance(importances, type = "rank", problem = "correlated", method = "CFI")


plot_importance(importances, problem = "independent", method = "mSAGE")
plot_importance(importances, problem = "independent", method = "cSAGE")
plot_importance(importances, problem = "independent", method = "mSAGE")
plot_importance(importances, problem = "independent", method = "mSAGE", learner_type = "boosting")
plot_importance(importances, problem = "independent", learner_type = "boosting", facets = "method")

plot_importance(importances, type = "rank", problem = "confounded", method = "mSAGE")
plot_importance(importances, type = "rank", problem = "confounded", method = "cSAGE")

plot_importance(importances, type = "rank", problem = "independent", method = "mSAGE")
plot_importance(importances, type = "rank", problem = "independent", method = "cSAGE")


for (problem in unique(importances$problem)) {
	for (method in unique(importances$method)) {
		# plot_importance(importances, problem = "independent", method = "mSAGE",  color = "package", facets = "learner_type")
		plot_importance(
			importances,
			problem = problem,
			method = method,
			color = "package",
			facets = "learner_type",
			caption = FALSE
		) |>
			save_plot(
				name = glue::glue("importance-{problem}-{method}"),
				plot_path = here::here("plots", "importance", "all-learners"),
				height = 6,
				width = 9
			)
	}
}

# One by one also for learner types
for (problem in unique(importances$problem)) {
	for (method in unique(importances$method)) {
		for (learner_type in unique(importances$learner_type)) {
			# plot_importance(importances, problem = "independent", method = "mSAGE",  color = "package", facets = "learner_type")
			plot_importance(
				importances,
				problem = problem,
				method = method,
				learner_type = learner_type,
				color = "package",
				facets = NULL,
				caption = TRUE
			) |>
				save_plot(
					name = glue::glue("importance-{problem}-{method}-{learner_type}"),
					plot_path = here::here("plots", "importance", "by-learner"),
					height = 5,
					width = 9
				)
		}
	}
}

# xplainfi only
for (problem in unique(importances$problem)) {
	# plot_importance(
	# 	importances[package == "xplainfi"],
	# 	problem = "independent",
	# 	method = NULL,
	# 	color = "method",
	# 	facets = "learner_type",
	# 	subtitle = FALSE,
	# 	caption = FALSE
	# )
	plot_importance(
		importances[package == "xplainfi"],
		problem = problem,
		method = NULL,
		color = "method",
		facets = "learner_type",
		subtitle = FALSE,
		caption = FALSE
	) |>
		save_plot(
			name = glue::glue("importance-{problem}-{method}"),
			plot_path = here::here("plots", "importance", "xplainfi-only"),
			height = 6,
			width = 9
		)
}


# importances |>
# 	dplyr::filter(
# 		problem == "bike_sharing",
# 		method == "CFI",
# 		learner_type == "boosting"
# 		# is.na(converged) | converged == FALSE
# 	) |>
# 	# dplyr::mutate(method = )
# 	ggplot(aes(x = importance_scaled, y = feature, fill = package, color = package)) +
# 	facet_wrap(vars(learner_type), dir = "h") +
# 	geom_boxplot(alpha = 3 / 4) +
# 	scale_x_continuous(labels = scales::label_percent()) +
# 	scale_fill_brewer(palette = "Dark2", aesthetics = c("fill", "color")) +
# 	labs(
# 		title = "Problem: 'interactions'",
# 		subtitle = "Method 'mSAGE' by learner type",
# 		x = "Importance (scaled, %)",
# 		y = "Feature",
# 		color = NULL,
# 		fill = NULL
# 	) +
# 	theme_minimal(base_size = 14) +
# 	theme(legend.position = "top", plot.title.position = "plot")

# importances |>
# 	dplyr::filter(
# 		problem == "interactions",
# 		method == "mSAGE",
# 		package == "xplainfi",
# 		learner_type == "rf",
# 		feature %in% c("x2")
# 	) |>
# 	dplyr::select(
# 		job.id,
# 		scores,
# 		runtime,
# 		learner_performance,
# 		converged,
# 		n_permutations_used,
# 		importance
# 	)

# Tables -----------------------------------------------------------------

table_base <- importances |>
	# mutate(
	# 	problem = ifelse(
	# 		problem == "correlated",
	# 		glue::glue("correlated (r={correlation})"),
	# 		problem
	# 	)
	# ) |>
	# filter(problem == "independent", learner_type == "boosting") |>
	summarize(
		min = min(importance_scaled),
		max = max(importance_scaled),
		mean = mean(importance_scaled),
		median = median(importance_scaled),
		sd = sd(importance_scaled),
		q25 = quantile(importance_scaled, prob = 0.25),
		q75 = quantile(importance_scaled, prob = 0.75),
		.by = c("problem", "method", "package", "learner_type", "feature")
	) |>
	mutate(across(where(is.numeric), \(x) round(x * 100, 2))) |>
	mutate(
		meansd = glue::glue("{mean} ({sd})"),
		medianq = glue::glue("{median} ({q25}, {q75})")
	) |>
	select(problem, method, package, learner_type, feature, meansd, medianq) |>
	arrange(feature)


table_base |>
	filter(problem == "correlated (r=0.25)", learner_type == "boosting", method == "PFI") |>
	tidyr::pivot_wider(
		id_cols = c("problem", "feature", "method"),
		names_from = package,
		values_from = "meansd"
	)

table_base |>
	filter(problem == "bike_sharing", learner_type == "boosting", method == "mSAGE") |>
	tidyr::pivot_wider(
		id_cols = c("problem", "feature", "method"),
		names_from = package,
		values_from = "meansd"
	)

importances |>
	select(problem, feature, method, learner_type, importance_scaled) |>
	slice_sample(n = 6)
