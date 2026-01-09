#' Plot saving utility
#' @param p A ggplot2 plot.
#' @param name character(1) Prefix for the filename.
#' @param plot_path character(1) DIrectory path to store plot. Will be created if not existing.
#' @param height,width numeric(1) Plot dimensions in inches.
#' @param formats character() One or more formats to store plot as, defaulting to `"png"`.
#' @param dpi integer(1) Image DPI
save_plot = function(
	p,
	name,
	plot_path = here::here("plots"),
	height = 6,
	width = 9,
	formats = "png",
	dpi = 300
) {
	# if (interactive()) {
	# 	print(p)
	# }

	for (format in formats) {
		filename = fs::path(plot_path, name, ext = format)
		cli::cli_alert_info("Saving {.file {fs::path_rel(filename)}} / {.val {format}}")

		ggsave(
			filename = filename,
			plot = p,
			width = width,
			height = height,
			dpi = dpi,
			bg = "white"
		)
	}
}


# Importance -------------------------------------------------------------

#' Aggregate results from batchtools registry
#' @param results Result table as returned by reduceResultsDataTable(). Read from `here::here("results", "importance", "results.rds")` if NULL.
aggregate_results_importance <- function(results, job_pars) {
	tmpres = data.table::rbindlist(results$result, fill = TRUE)
	tmpres = cbind(results[, .(job.id)], tmpres)
	res = ijoin(
		tmpres,
		job_pars[, .(
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
		)],
		by = "job.id"
	)

	# Metadata for grouping
	res[, let(
		method = fcase(
			startsWith(algorithm, "PFI")             , "PFI"   ,
			startsWith(algorithm, "CFI")             , "CFI"   ,
			startsWith(algorithm, "LOCO")            , "LOCO"  ,
			startsWith(algorithm, "MarginalSAGE")    , "mSAGE" ,
			startsWith(algorithm, "ConditionalSAGE") , "cSAGE"
		),
		package = fcase(
			endsWith(algorithm, "_iml")   , "iml"   ,
			endsWith(algorithm, "_vip")   , "vip"   ,
			endsWith(algorithm, "_fippy") , "fippy" ,
			endsWith(algorithm, "_sage")  , "sage"  ,
			default = "xplainfi"
		)
	)]
	res[, package := factor(package, levels = c("xplainfi", "fippy", "vip", "iml", "sage"))]
	res[, method := factor(method, levels = c("PFI", "CFI", "mSAGE", "cSAGE", "LOCO"))]
	res[, learner_type := factor(learner_type, levels = c("linear", "rf", "boosting", "mlp"))]
	res[, algorithm_lab := sprintf("%s (%s)", method, package)]
	res[,
		algorithm_lab := factor(
			algorithm_lab,
			levels = c(
				"PFI (xplainfi)",
				"PFI (iml)",
				"PFI (vip)",
				"CFI (xplainfi)",
				"CFI (fippy)",
				"PFI (fippy)",
				"mSAGE (xplainfi)",
				"mSAGE (sage)",
				"mSAGE (fippy)",
				"cSAGE (xplainfi)",
				"cSAGE (fippy)",
				"LOCO (xplainfi)"
			)
		)
	]

	# res |> dplyr::count(algorithm, method, package, sampler)

	# Extract importances
	importances = rbindlist(
		lapply(results$job.id, \(x) {
			importances = results[job.id == x, result[[1]]$importance]
			importances[, job.id := x]
		}),
		fill = TRUE
	)
	# Add job parameters (algorithm, problem parameters, ...)
	importances = merge(res[, -"importance"], importances, by = "job.id")

	importances[,
		importance_scaled := (importance - min(importance)) / (max(importance) - min(importance)),
		by = .(algorithm, learner_type, problem, correlation)
	][]
}


plot_importance <- function(
	importances,
	...,
	problem = NULL,
	method = NULL,
	learner_type = NULL,
	feature = NULL,
	color = "package",
	facets = "learner_type",
	ncol = NULL,
	nrow = NULL,
	subtitle = TRUE,
	caption = TRUE
) {
	checkmate::assert_subset(problem, as.character(unique(importances$problem)))
	checkmate::assert_subset(method, as.character(unique(importances$method)))
	checkmate::assert_subset(learner_type, as.character(unique(importances$learner_type)))
	checkmate::assert_subset(color, names(importances))
	checkmate::assert_subset(facets, names(importances))

	# subtitle <- NULL
	# if (!is.null(method)) {
	# 	subtitle <- glue::glue("Method: {method_lab}")
	# }

	# caption <- NULL
	# if (!is.null(learner_type)) {
	# 	caption <- glue::glue("Learner: {learner_type}")
	# }

	problem <- problem %||% unique(importances$problem)
	method <- method %||% unique(importances$method)
	learner_type <- learner_type %||% unique(importances$learner_type)
	feature <- feature %||% unique(importances$feature)

	problem_lab <- glue::glue_collapse(problem, sep = ", ", last = ", and ")
	method_lab <- glue::glue_collapse(method, sep = ", ", last = ", and ")
	learner_type_lab <- glue::glue_collapse(learner_type, sep = ", ", last = ", and ")

	cli::cli_alert_info("Problem = {.val {problem}}")
	cli::cli_alert_info("Method = {.val {method}}")
	cli::cli_alert_info("Learner = {.val {learner_type}}")

	subtitle_lab <- if (subtitle && !is.null(method)) glue::glue("Method: {method_lab}")
	caption_lab <- if (caption && !is.null(learner_type)) glue::glue("Learner: {learner_type_lab}")

	p <- importances |>
		dplyr::filter(
			.data[["problem"]] %in% .env[["problem"]],
			.data[["method"]] %in% .env[["method"]],
			.data[["learner_type"]] %in% .env[["learner_type"]],
			.data[["feature"]] %in% .env[["feature"]],
			...
		) |>
		dplyr::mutate(feature = forcats::fct_reorder(feature, importance_scaled)) |>
		ggplot(aes(x = importance_scaled, y = feature, color = .data[[color]], fill = .data[[color]])) +
		geom_boxplot(alpha = 3 / 4) +
		scale_x_continuous(labels = scales::label_percent()) +
		labs(
			title = glue::glue("Problem: {problem_lab}"),
			subtitle = subtitle_lab,
			x = "Importance (scaled, %)",
			y = "Feature",
			color = NULL,
			fill = NULL,
			caption = caption_lab
		) +
		theme_minimal(base_size = 14) +
		theme(legend.position = "top", plot.title.position = "plot")

	if (!is.null(color)) {
		if (color == "package") {
			p <- p + scale_fill_manual(values = pal_package, aesthetics = c("color", "fill"))
		} else {
			if (length(unique(importances[[color]])) <= 5) {
				p <- p + scale_fill_brewer(palette = "Dark2", aesthetics = c("fill", "color"))
			} else {
				p <- p + scale_fill_viridis_d(aesthetics = c("fill", "color"))
			}
		}
	}

	if (length(facets) > 0) {
		p <- p + facet_wrap(facets = facets, dir = "h", ncol = ncol, nrow = nrow)
	}

	p
}

# Runtime ----------------------------------------------------------------

aggregate_results_runtime <- function(results, job_pars) {
	tmpres <- data.table::rbindlist(results$result, fill = TRUE)
	tmpres <- cbind(results[, .(job.id)], tmpres)
	tmpres[, scores := NULL]

	res <- ijoin(
		tmpres,
		job_pars[, .(
			job.id,
			algorithm,
			learner_type,
			n_repeats,
			sampler,
			n_permutations,
			sage_n_samples
		)],
		by = "job.id"
	)

	# Metadata for grouping
	res[, let(
		method = fcase(
			startsWith(algorithm, "PFI")             , "PFI"   ,
			startsWith(algorithm, "CFI")             , "CFI"   ,
			startsWith(algorithm, "LOCO")            , "LOCO"  ,
			startsWith(algorithm, "MarginalSAGE")    , "mSAGE" ,
			startsWith(algorithm, "ConditionalSAGE") , "cSAGE"
		),
		package = fcase(
			endsWith(algorithm, "_iml")   , "iml"   ,
			endsWith(algorithm, "_vip")   , "vip"   ,
			endsWith(algorithm, "_fippy") , "fippy" ,
			endsWith(algorithm, "_sage")  , "sage"  ,
			default = "xplainfi"
		)
	)]
	res[, package := factor(package, levels = c("xplainfi", "fippy", "vip", "iml", "sage"))]
	res[, method := factor(method, levels = c("PFI", "CFI", "mSAGE", "cSAGE", "LOCO"))]
	res[, learner_type := factor(learner_type, levels = c("linear", "featureless"))]
	res[, algorithm_lab := sprintf("%s (%s)", method, package)]
	res[,
		algorithm_lab := factor(
			algorithm_lab,
			levels = c(
				"PFI (xplainfi)",
				"PFI (iml)",
				"PFI (vip)",
				"CFI (xplainfi)",
				"CFI (fippy)",
				"PFI (fippy)",
				"mSAGE (xplainfi)",
				"mSAGE (sage)",
				"mSAGE (fippy)",
				"cSAGE (xplainfi)",
				"cSAGE (fippy)",
				"LOCO (xplainfi)"
			)
		)
	]

	res[]
}
