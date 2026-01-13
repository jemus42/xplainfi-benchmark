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
			repl,
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

	# Minor issue where learner_performance var was incorrectly labelled in vip and iml
	if ("learner_performance.regr.rsq" %in% names(res)) {
		res[,
			learner_performance := fifelse(
				is.na(learner_performance),
				learner_performance.regr.rsq, # in vip, iml
				learner_performance # Regular variable name otherwise
			)
		]
		res[, learner_performance.regr.rsq := NULL]
	}

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
	feature_sort = c("importance", "name"),
	base_size = 14
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
		theme_minimal(base_size = base_size) +
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

#' Plot runtime as box plots with algorithm on the y-axis and runtime on the x-axis.
#'
#' @param runtimes data.table of runtime data, produced by `aggregate_results_runtime()`
#' @param scale character(1) Scale type: "seconds", "log10 seconds", "relative", or "log10 relative".
#'   Relative scales compute runtime relative to xplainfi within each replication.
#' @param method,package,learner_type,sampler character() Filter by these variables. NULL uses all.
#' @param n_samples,n_features,n_permutations,sage_n_samples Filter by these numeric variables. NULL uses all.
#' @param color character(1) Variable name to color by.
#' @param facets character() Variable names to facet by.
#' @param ncol,nrow integer(1) Passed to `facet_wrap()`.
#' @param show_legend logical(1) Whether to show the legend.
#' @param base_size numeric(1) Base font size for theme.
#' @return A ggplot2 object.
plot_runtime <- function(
	runtimes,
	scale = c("seconds", "log10 seconds", "relative", "log10 relative"),
	method = NULL,
	package = NULL,
	learner_type = NULL,
	sampler = NULL,
	n_samples = NULL,
	n_features = NULL,
	n_permutations = NULL,
	sage_n_samples = NULL,
	color = "package",
	facets = c("n_samples", "n_features"),
	ncol = 2,
	nrow = NULL,
	show_legend = TRUE,
	base_size = 16
) {
	scale <- match.arg(scale)
	data <- data.table::copy(runtimes)

	# Apply filters
	if (!is.null(method)) {
		data <- data[method %in% .env$method]
	}
	if (!is.null(package)) {
		data <- data[package %in% .env$package]
	}
	if (!is.null(learner_type)) {
		data <- data[learner_type %in% .env$learner_type]
	}
	if (!is.null(sampler)) {
		data <- data[is.na(sampler) | sampler %in% .env$sampler]
	}
	if (!is.null(n_samples)) {
		data <- data[n_samples %in% .env$n_samples]
	}
	if (!is.null(n_features)) {
		data <- data[n_features %in% .env$n_features]
	}
	if (!is.null(n_permutations)) {
		data <- data[is.na(n_permutations) | n_permutations %in% .env$n_permutations]
	}
	if (!is.null(sage_n_samples)) {
		data <- data[is.na(sage_n_samples) | sage_n_samples %in% .env$sage_n_samples]
	}

	if (nrow(data) == 0) {
		cli::cli_warn("No data remaining after filtering")
		return(
			ggplot() +
				annotate("text", x = 0.5, y = 0.5, label = "No data for selection", size = 6) +
				theme_void()
		)
	}

	# Log filter selections
	cli::cli_alert_info("Methods: {.val {unique(data$method)}}")
	cli::cli_alert_info("Packages: {.val {unique(data$package)}}")
	cli::cli_alert_info("Learners: {.val {unique(data$learner_type)}}")
	cli::cli_alert_info("n_samples: {.val {sort(unique(data$n_samples))}}")
	cli::cli_alert_info("n_features: {.val {sort(unique(data$n_features))}}")

	# Set plot variable and axis label based on scale type
	plot_var <- "runtime"
	x_lab <- switch(
		scale,
		"seconds" = "Runtime (seconds)",
		"log10 seconds" = "Runtime (seconds, log scale)",
		"relative" = "Runtime (relative to xplainfi)",
		"log10 relative" = "Runtime (relative to xplainfi, log scale)"
	)

	# Compute relative runtime if needed
	if (scale %in% c("relative", "log10 relative")) {
		group_cols <- c(
			"method",
			"learner_type",
			"n_samples",
			"n_features",
			"sampler",
			"n_permutations",
			"sage_n_samples",
			"repl"
		)
		group_cols <- intersect(group_cols, names(data))

		baseline <- data[
			package == "xplainfi",
			.(baseline_runtime = runtime[1]),
			by = group_cols
		]

		data <- merge(data, baseline, by = group_cols, all.x = TRUE)
		data[, runtime_relative := runtime / baseline_runtime]
		plot_var <- "runtime_relative"
	}

	# Convert numeric columns to factors for proper faceting/coloring
	data[, n_samples := factor(n_samples)]
	data[, n_features := factor(n_features)]
	data[, n_permutations := factor(n_permutations)]
	data[, sage_n_samples := factor(sage_n_samples)]

	# Create algorithm label for y-axis
	data[, algo_label := sprintf("%s (%s)", method, package)]

	# Order by median runtime
	algo_order <- data[, .(med_rt = median(get(plot_var), na.rm = TRUE)), by = algo_label]
	algo_order <- algo_order[order(med_rt)]
	data[, algo_label := factor(algo_label, levels = algo_order$algo_label)]

	# Build plot
	p <- ggplot(
		data,
		aes(
			x = .data[[plot_var]],
			y = algo_label,
			fill = .data[[color]],
			color = .data[[color]]
		)
	) +
		geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
		labs(
			title = "Runtime Comparison",
			x = x_lab,
			y = NULL,
			fill = color,
			color = color
		) +
		theme_minimal(base_size = base_size) +
		theme(
			legend.position = if (show_legend) "top" else "none",
			plot.title.position = "plot"
		)

	# Color palette
	if (color == "package") {
		p <- p + scale_fill_manual(values = pal_package, aesthetics = c("fill", "color"))
	} else {
		p <- p + scale_fill_brewer(palette = "Dark2", aesthetics = c("fill", "color"))
	}

	# Add faceting
	if (length(facets) > 0) {
		p <- p + facet_wrap(facets = facets, ncol = ncol, nrow = nrow, labeller = label_both)
	}

	# X-axis scaling with pretty labels
	if (scale == "log10 seconds") {
		p <- p + scale_x_log10(labels = scales::label_number())
	} else if (scale == "relative") {
		p <- p +
			geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
			scale_x_continuous(labels = scales::label_number(suffix = "x"))
	} else if (scale == "log10 relative") {
		p <- p +
			geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
			scale_x_log10(labels = scales::label_number(suffix = "x"))
	} else {
		p <- p + scale_x_continuous(labels = scales::label_number())
	}

	p
}

# =============================================================================
# Paper-specific analysis helpers
# =============================================================================

#' Compute pairwise agreement metrics between xplainfi and reference packages
#'
#' Computes both value-based metrics (Pearson correlation, MAE, RMSE on scaled importance)
#' and rank-based metrics (Spearman, Kendall correlations on feature rankings).
#'
#' @param importances data.table from clean_results_importance()
#' @param method character() Methods to include. NULL uses all.
#' @param problem character() Problems to include. NULL uses all.
#' @return data.table with agreement metrics between xplainfi and each reference package,
#'   grouped by problem, method, and learner_type.
compute_agreement <- function(importances, method = NULL, problem = NULL) {
	data <- copy(importances)

	if (!is.null(method)) {
		data <- data[data$method %in% method]
	}
	if (!is.null(problem)) {
		data <- data[data$problem %in% problem]
	}

	if (nrow(data) == 0) {
		cli::cli_warn("No data remaining after filtering")
		return(data.table())
	}

	# Aggregate to mean importance per feature/problem/learner/method/package
	agg <- data[,
		.(
			mean_scaled = mean(importance_scaled, na.rm = TRUE),
			mean_rank = mean(importance_rank, na.rm = TRUE)
		),
		by = .(problem, feature, learner_type, method, package)
	]

	# Pivot scaled importance to wide format
	wide_scaled <- dcast(
		agg,
		problem + feature + learner_type + method ~ package,
		value.var = "mean_scaled"
	)

	# Pivot ranks to wide format
	wide_rank <- dcast(
		agg,
		problem + feature + learner_type + method ~ package,
		value.var = "mean_rank"
	)

	# Find reference packages (everything except xplainfi and grouping cols)
	group_cols <- c("problem", "feature", "learner_type", "method")
	ref_packages <- setdiff(names(wide_scaled), c(group_cols, "xplainfi"))

	if (length(ref_packages) == 0) {
		cli::cli_warn("No reference packages found for comparison")
		return(data.table())
	}

	# Helper to compute metrics for a given reference package
	compute_metrics_for_ref <- function(ref) {
		valid_scaled <- wide_scaled[!is.na(xplainfi) & !is.na(get(ref))]
		valid_rank <- wide_rank[!is.na(xplainfi) & !is.na(get(ref))]

		if (nrow(valid_scaled) == 0) {
			return(NULL)
		}

		# Compute value-based metrics
		value_metrics <- valid_scaled[,
			.(
				pearson = cor(xplainfi, get(ref), method = "pearson", use = "complete.obs"),
				mae = mean(abs(xplainfi - get(ref)), na.rm = TRUE),
				rmse = sqrt(mean((xplainfi - get(ref))^2, na.rm = TRUE)),
				n_features = .N
			),
			by = .(problem, method, learner_type)
		]

		# Compute rank-based metrics
		rank_metrics <- valid_rank[,
			.(
				spearman = cor(xplainfi, get(ref), method = "spearman", use = "complete.obs"),
				kendall = cor(xplainfi, get(ref), method = "kendall", use = "complete.obs")
			),
			by = .(problem, method, learner_type)
		]

		# Merge and add reference column
		result <- merge(value_metrics, rank_metrics, by = c("problem", "method", "learner_type"))
		result[, reference := ref]
		result
	}

	metrics <- rbindlist(lapply(ref_packages, compute_metrics_for_ref))
	setcolorder(metrics, c("problem", "method", "learner_type", "reference"))
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
			sage_n_samples,
			repl
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
