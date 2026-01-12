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
clean_results_importance <- function(results, job_pars) {
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
		problem_clean := fcase(
			problem == "correlated"   , sprintf("correlated (r=%.2f)", correlation) ,
			problem == "bike_sharing" , "bike sharing"                              ,
			default = problem
		)
	]
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
		# Scaled to unit interval, within each job
		importance_scaled := (importance - min(importance)) / (max(importance) - min(importance)),
		by = .(job.id, algorithm, learner_type, problem, correlation)
	]
	# Ranks are also computed within each job
	importances[,
		# Rank such that highest importance -> rank 1
		importance_rank := frank(-importance, ties.method = "average"),
		by = .(job.id, algorithm, learner_type, problem, correlation)
	]

	checkmate::assert_numeric(importances$importance_scaled, lower = 0, upper = 1)
	# ranks should be 1 to p, bike sharing is problem with highest feature count, 12
	checkmate::assert_numeric(
		importances$importance_rank,
		lower = 1,
		upper = max(importances[, .(p = uniqueN(feature)), by = .(problem)][, p])
	)

	importances[]
}


# Plotting ---------------------------------------------------------------

# Color palette for packages
pal_package <- c(
	xplainfi = "#1e3888",
	fippy = "#8CD867",
	vip = "#A31621",
	iml = "#ffad69",
	sage = "#4E878C"
)

#' Plot importances as box plots with feature on the y-axis and importance on the x-axis.
#'
#' @param importances ta.table of importance, produced by `clean_results_importance()`
#' @param type Type of importance, one of `"raw"` importances, `"scaled"` to [0, 1] (default), or `"rank"` (1 = most important).
#' @param problem,method,learner_type,feature character() specification of experiment parameter to select (one or more).
#'   If `NULL`, all available are used simulatenously.
#' @param color character(1): `"package"` Variable name to set as `color` (and `fill`) aesthetic in `ggplot2::aes()`
#' @param facets character(): `learner_type` Variable name(s) to facet by via `ggplot2::facet_wrap()`.
#' @param ncol,nrow integer(1) passed to `facet_wrap()`.
#' @param subtitle,caption logical(1): `TRUE` Toggle subtitle (showing method label) or caption (showing learner_type).
plot_importance <- function(
	importances,
	type = c("scaled", "raw", "rank"),
	problem = NULL,
	method = NULL,
	learner_type = NULL,
	feature = NULL,
	color = "package",
	facets = "learner_type",
	ncol = NULL,
	nrow = NULL,
	subtitle = TRUE,
	caption = TRUE,
	feature_sort = c("importance", "name")
) {
	checkmate::assert_subset(problem, as.character(unique(importances$problem)))
	checkmate::assert_subset(method, as.character(unique(importances$method)))
	checkmate::assert_subset(learner_type, as.character(unique(importances$learner_type)))
	checkmate::assert_subset(color, names(importances))
	checkmate::assert_subset(facets, names(importances))
	type <- match.arg(type)
	feature_sort <- match.arg(feature_sort)

	target_var <- switch(
		type,
		raw = "importance",
		scaled = "importance_scaled",
		rank = "importance_rank"
	)

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

	importance_subset <- importances |>
		dplyr::filter(
			.data[["problem"]] %in% .env[["problem"]],
			.data[["method"]] %in% .env[["method"]],
			.data[["learner_type"]] %in% .env[["learner_type"]],
			.data[["feature"]] %in% .env[["feature"]]
		)

	if ("correlated" %in% problem) {
		problem[problem == "correlated"] <- sprintf(
			"%s (r=%s)",
			problem,
			paste0(unique(importances$correlation), collapse = ", ")
		)
	}

	problem_lab <- glue::glue_collapse(problem, sep = ", ", last = ", and ")
	method_lab <- glue::glue_collapse(method, sep = ", ", last = ", and ")
	learner_type_lab <- glue::glue_collapse(learner_type, sep = ", ", last = ", and ")

	cli::cli_alert_info("Problem = {.val {problem}}")
	cli::cli_alert_info("Method = {.val {method}}")
	cli::cli_alert_info("Learner = {.val {learner_type}}")

	subtitle_lab <- NULL
	if (subtitle && !is.null(method)) {
		subtitle_lab <- glue::glue("Method: {method_lab}")
	}
	caption_lab <- NULL
	if (caption && !is.null(learner_type)) {
		glue::glue("Learner: {learner_type_lab}")
	}

	x_lab <- "Importance"
	x_lab <- switch(
		type,
		scaled = paste0(x_lab, " (scaled, %)"),
		rank = paste0(x_lab, " (ranks)"),
		x_lab
	)

	if (feature_sort == "importance") {
		importance_subset <- importance_subset |>
			dplyr::mutate(feature = forcats::fct_reorder(feature, importance))
	} else if (feature_sort == "name") {
		importance_subset <- importance_subset |>
			dplyr::mutate(feature = forcats::fct_rev(feature))
	}

	p <- importance_subset |>
		ggplot(aes(
			x = .data[[target_var]],
			y = feature,
			color = .data[[color]],
			fill = .data[[color]]
		)) +
		geom_boxplot(alpha = 3 / 4) +
		labs(
			title = glue::glue("Problem: {problem_lab}"),
			subtitle = subtitle_lab,
			x = x_lab,
			y = "Feature",
			color = NULL,
			fill = NULL,
			caption = caption_lab
		) +
		theme_minimal(base_size = 14) +
		theme(legend.position = "top", plot.title.position = "plot")

	if (type == "scaled") {
		p <- p + scale_x_continuous(labels = scales::label_percent())
	}

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

# =============================================================================
# Paper-specific analysis helpers
# =============================================================================

#' Compute pairwise agreement metrics between packages
#'
#' @param importances data.table from clean_results_importance()
#' @param method_filter character Method to filter (PFI, CFI, mSAGE, cSAGE, LOCO)
#' @return data.table with correlation, MAE, RMSE between xplainfi and each reference
compute_package_agreement <- function(importances, method_filter = NULL) {
	data <- copy(importances)

	if (!is.null(method_filter)) {
		data <- data[method %in% method_filter]
	}

	# Aggregate to mean importance per feature/problem/learner
	agg <- data[,
		.(mean_imp = mean(importance_scaled)),
		by = .(problem, feature, learner_type, method, package)
	]

	# Pivot to wide format
	wide <- dcast(
		agg,
		problem + feature + learner_type + method ~ package,
		value.var = "mean_imp"
	)

	# Compute metrics against xplainfi
	ref_packages <- setdiff(
		names(wide),
		c("problem", "feature", "learner_type", "method", "xplainfi")
	)

	metrics <- rbindlist(lapply(ref_packages, function(ref) {
		valid <- wide[!is.na(xplainfi) & !is.na(get(ref))]
		if (nrow(valid) == 0) {
			return(NULL)
		}

		valid[,
			.(
				reference = ref,
				correlation = cor(xplainfi, get(ref), use = "complete.obs"),
				mae = mean(abs(xplainfi - get(ref)), na.rm = TRUE),
				rmse = sqrt(mean((xplainfi - get(ref))^2, na.rm = TRUE)),
				n_comparisons = .N
			),
			by = .(method, learner_type)
		]
	}))

	metrics[]
}

#' Compute rank correlation (Spearman) for feature rankings
#'
#' @param importances data.table from clean_results_importance()
#' @return data.table with Spearman correlations between packages
compute_rank_agreement <- function(importances) {
	# Aggregate to mean importance and compute ranks
	agg <- importances[,
		.(mean_imp = mean(importance_scaled)),
		by = .(problem, feature, learner_type, method, package)
	]

	# Add rank within each problem/learner/method/package combination
	agg[,
		rank := rank(-mean_imp, ties.method = "average"),
		by = .(problem, learner_type, method, package)
	]

	# Pivot ranks to wide format
	wide <- dcast(
		agg,
		problem + feature + learner_type + method ~ package,
		value.var = "rank"
	)

	ref_packages <- setdiff(
		names(wide),
		c("problem", "feature", "learner_type", "method", "xplainfi")
	)

	metrics <- rbindlist(lapply(ref_packages, function(ref) {
		valid <- wide[!is.na(xplainfi) & !is.na(get(ref))]
		if (nrow(valid) == 0) {
			return(NULL)
		}

		valid[,
			.(
				reference = ref,
				spearman = cor(xplainfi, get(ref), method = "spearman", use = "complete.obs"),
				kendall = cor(xplainfi, get(ref), method = "kendall", use = "complete.obs"),
				n_features = .N
			),
			by = .(problem, learner_type, method)
		]
	}))

	metrics[]
}

#' Create a LaTeX-ready comparison table
#'
#' @param importances data.table from clean_results_importance()
#' @param problem_filter character Problem to filter
#' @param method_filter character Method to filter
#' @param learner_filter character Learner type to filter
#' @return data.table formatted for LaTeX export
create_latex_table <- function(
	importances,
	problem_filter,
	method_filter,
	learner_filter = NULL
) {
	data <- copy(importances)[
		problem == problem_filter & method == method_filter
	]

	if (!is.null(learner_filter)) {
		data <- data[learner_type %in% learner_filter]
	}

	# Summary statistics
	summary_dt <- data[,
		.(
			mean = mean(importance_scaled),
			sd = sd(importance_scaled),
			median = median(importance_scaled),
			q25 = quantile(importance_scaled, 0.25),
			q75 = quantile(importance_scaled, 0.75)
		),
		by = .(feature, package, learner_type)
	]

	# Format for LaTeX
	summary_dt[, value := sprintf("%.1f (%.1f)", mean * 100, sd * 100)]

	# Pivot to wide
	result <- dcast(
		summary_dt,
		feature + learner_type ~ package,
		value.var = "value"
	)

	setorder(result, learner_type, -mean)
	result[]
}

#' Compute variance decomposition: how much variance is explained by each factor
#'
#' @param importances data.table from clean_results_importance()
#' @param package_filter character Package to analyze (default: "xplainfi")
#' @return data.table with variance components
compute_variance_decomposition <- function(importances, package_filter = "xplainfi") {
	data <- copy(importances)[package == package_filter]

	# Simple variance decomposition using coefficient of variation
	components <- data[,
		.(
			var_total = var(importance_scaled),
			var_by_problem = var(tapply(importance_scaled, problem, mean)),
			var_by_learner = var(tapply(importance_scaled, learner_type, mean)),
			var_by_method = var(tapply(importance_scaled, method, mean)),
			var_by_feature = var(tapply(importance_scaled, feature, mean))
		),
		by = NULL
	]

	components
}

#' Summarize learner performance from benchmark results
#'
#' @param results data.table Results from reduceResultsDataTable
#' @param job_pars data.table Job parameters
#' @return data.table with learner performance metrics
summarize_learner_performance <- function(results, job_pars) {
	# Extract learner performance from results
	perf <- rbindlist(
		lapply(seq_len(nrow(results)), function(i) {
			lp <- results$result[[i]]$learner_performance
			if (is.null(lp)) {
				return(NULL)
			}
			data.table(
				job.id = results$job.id[i],
				learner_performance = lp
			)
		}),
		fill = TRUE
	)

	if (nrow(perf) == 0) {
		return(NULL)
	}

	# Join with job parameters
	perf <- merge(
		perf,
		job_pars[, .(job.id, problem, algorithm, learner_type)],
		by = "job.id"
	)

	# Summarize by problem and learner type
	perf[,
		.(
			mean_perf = mean(learner_performance, na.rm = TRUE),
			sd_perf = sd(learner_performance, na.rm = TRUE),
			n = .N
		),
		by = .(problem, learner_type)
	]
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
