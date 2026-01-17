# Simple Shiny app for exploring importance benchmark results
# Run with: shiny::runApp("R/shiny-importance.R")

library(shiny)
library(data.table)
library(ggplot2)
library(bslib)

# Load data and helper functions
source(here::here("R", "analysis.R"))

file_importance <- fs::path(here::here("results", "importance"), "importances", ext = "rds")
if (!fs::file_exists(file_importance)) {
	stop("Run analysis-importance.R first to generate importances.rds")
}
importances <- readRDS(file_importance)


# UI -------------------------------------------------------------------------

ui <- page_sidebar(
	title = "xplainfi Benchmark Results Explorer",
	theme = bs_theme(bootswatch = "flatly"),

	sidebar = sidebar(
		width = 300,

		selectInput(
			"problem",
			"Problem",
			choices = unique(importances$problem),
			selected = "friedman1"
		),

		conditionalPanel(
			condition = "input.problem == 'correlated'",
			selectInput(
				"correlation",
				"Correlation",
				choices = c("0.25", "0.75", "both"),
				selected = "both"
			)
		),

		selectInput(
			"method",
			"Method",
			choices = levels(importances$method),
			selected = "PFI",
			multiple = TRUE
		),

		selectInput(
			"package",
			"Package",
			choices = levels(importances$package),
			selected = levels(importances$package),
			multiple = TRUE
		),

		selectInput(
			"learner_type",
			"Learner Type",
			choices = levels(importances$learner_type),
			selected = levels(importances$learner_type),
			multiple = TRUE
		),

		selectInput(
			"feature",
			"Features",
			choices = unique(importances$feature),
			selected = unique(importances$feature),
			multiple = TRUE
		),

		conditionalPanel(condition = "input.method.includes('mSAGE') || input.method.includes('cSAGE')", {
			perm_vals <- na.omit(importances$n_permutations_used)
			perm_min <- if (length(perm_vals) > 0) min(perm_vals) else 1
			perm_max <- if (length(perm_vals) > 0) max(perm_vals) else 100
			sliderInput(
				"n_permutations_used_range",
				"n_permutations_used (SAGE)",
				min = perm_min,
				max = perm_max,
				value = c(perm_min, perm_max),
				step = 1
			)
		}),

		hr(),

		selectInput(
			"color_by",
			"Color by",
			choices = c("package", "method", "learner_type"),
			selected = "package"
		),

		selectInput(
			"facet_by",
			"Facet by",
			choices = c("learner_type", "method", "package", "feature"),
			selected = "learner_type",
			multiple = TRUE
		),

		selectInput(
			"y_var",
			"Y axis",
			choices = c("feature", "package", "method", "learner_type", "algorithm_lab"),
			selected = "feature"
		),

		selectInput(
			"importance_type",
			"Importance type",
			choices = c("scaled", "raw", "rank"),
			selected = "scaled"
		),

		radioButtons(
			"feature_sort",
			"Sort features by",
			choices = c("importance", "name"),
			selected = "importance",
			inline = TRUE
		),

		sliderInput(
			"base_size",
			"Text size",
			min = 10,
			max = 24,
			value = 14,
			step = 1
		),

		hr(),

		actionButton("update_plot", "Update Plot", class = "btn-primary"),

		hr(),

		h6("Data summary"),
		verbatimTextOutput("data_summary")
	),

	card(
		card_header("Importance Plot"),
		plotOutput("importance_plot", height = "800px")
	),

	card(
		card_header("Quality Info"),
		verbatimTextOutput("quality_info")
	)
)

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
	# Update available packages based on selected method
	observeEvent(input$method, {
		available_pkgs <- unique(importances[method %in% input$method, package])
		updateSelectInput(
			session,
			"package",
			choices = levels(importances$package),
			selected = intersect(input$package, as.character(available_pkgs))
		)
	})

	# Update facet/color options and features based on problem
	observeEvent(input$problem, {
		base_choices <- c("learner_type", "method", "package", "feature")
		color_choices <- c("package", "method", "learner_type")

		if (input$problem == "correlated") {
			facet_choices <- c(base_choices, "correlation")
			color_choices <- c(color_choices, "correlation")
		} else {
			facet_choices <- base_choices
		}

		updateSelectInput(
			session,
			"facet_by",
			choices = facet_choices,
			selected = intersect(input$facet_by, facet_choices)
		)

		# Update available features based on selected problem
		available_features <- unique(as.character(importances[problem == input$problem, feature]))
		updateSelectInput(
			session,
			"feature",
			choices = available_features,
			selected = available_features
		)
		updateSelectInput(
			session,
			"color_by",
			choices = color_choices,
			selected = if (input$color_by %in% color_choices) input$color_by else "package"
		)
	})

	# Reactive filtered data
	filtered_data <- reactive({
		req(input$problem, input$method, input$package, input$learner_type, input$feature)

		data <- importances[
			problem == input$problem &
				method %in% input$method &
				package %in% input$package &
				learner_type %in% input$learner_type &
				feature %in% input$feature
		]

		# Filter by correlation for correlated problem
		# Skip filter if correlation is used for faceting or coloring (need all values)
		correlation_in_viz <- "correlation" %in% c(input$facet_by, input$color_by)
		if (input$problem == "correlated" && input$correlation != "both" && !correlation_in_viz) {
			data <- data[correlation == as.numeric(input$correlation)]
		}

		# Filter by n_permutations_used range for SAGE methods
		if (any(input$method %in% c("mSAGE", "cSAGE")) && !is.null(input$n_permutations_used_range)) {
			perm_min <- input$n_permutations_used_range[1]
			perm_max <- input$n_permutations_used_range[2]
			data <- data[
				is.na(n_permutations_used) |
					(n_permutations_used >= perm_min & n_permutations_used <= perm_max)
			]
		}

		data
	})

	# Data summary
	output$data_summary <- renderPrint({
		data <- filtered_data()
		cat(sprintf("Observations: %d\n", nrow(data)))
		cat(sprintf("Jobs: %d\n", uniqueN(data$job.id)))
		cat(sprintf("Features: %d\n", uniqueN(data$feature)))
		cat(sprintf("Methods: %s\n", paste(unique(data$method), collapse = ", ")))
		cat(sprintf("Packages: %s\n", paste(unique(data$package), collapse = ", ")))
		# Show n_permutations_used if SAGE methods present
		if (any(data$method %in% c("mSAGE", "cSAGE"))) {
			perms <- sort(unique(na.omit(data$n_permutations_used)))
			if (length(perms) > 0) {
				cat(sprintf("n_permutations_used: %d - %d\n", min(perms), max(perms)))
			}
		}
	})

	# Quality info
	output$quality_info <- renderPrint({
		data <- filtered_data()

		# Learner performance by package (R vs Python learners differ)
		perf <- data[,
			.(mean_perf = mean(learner_performance, na.rm = TRUE)),
			by = .(learner_type, package)
		]
		setorder(perf, learner_type, package)
		cat("Learner Performance (R²):\n")
		for (i in seq_len(nrow(perf))) {
			status <- if (!is.na(perf$mean_perf[i]) && perf$mean_perf[i] < 0.5) " ⚠ LOW" else ""
			cat(sprintf(
				"  %s/%s: %.3f%s\n",
				perf$learner_type[i],
				perf$package[i],
				perf$mean_perf[i],
				status
			))
		}

		# SAGE convergence
		if (any(data$method %in% c("mSAGE", "cSAGE"))) {
			cat("\nSAGE Convergence:\n")
			conv <- data[
				method %in% c("mSAGE", "cSAGE") & package == "xplainfi",
				.(pct = round(100 * mean(converged, na.rm = TRUE), 1)),
				by = .(method, learner_type)
			]
			for (i in seq_len(nrow(conv))) {
				status <- if (!is.na(conv$pct[i]) && conv$pct[i] < 50) " ⚠ LOW" else ""
				cat(sprintf(
					"  %s/%s: %.1f%%%s\n",
					conv$method[i],
					conv$learner_type[i],
					conv$pct[i],
					status
				))
			}
		}
	})

	# Main plot
	output$importance_plot <- renderPlot({
		input$update_plot # Trigger on button click

		isolate({
			data <- filtered_data()

			if (nrow(data) == 0) {
				ggplot() +
					annotate("text", x = 0.5, y = 0.5, label = "No data for selection", size = 6) +
					theme_void()
			} else {
				# Convert correlation to factor for proper coloring/faceting
				if ("correlation" %in% names(data) && input$problem == "correlated") {
					data[, correlation := factor(correlation)]
				}

				# Use plot_importance() - data is already filtered by package/correlation
				# To recreate this plot outside Shiny:
				# plot_importance(
				#   importances[package %in% c(...) & ...],
				#   type = "...", color = "...", facets = c(...)
				# )
				plot_importance(
					data,
					type = input$importance_type,
					problem = NULL,
					method = NULL,
					learner_type = NULL,
					y_var = input$y_var,
					color = input$color_by,
					facets = input$facet_by,
					subtitle = FALSE,
					caption = FALSE,
					feature_sort = input$feature_sort,
					base_size = input$base_size
				)
			}
		})
	})
}

# Run app --------------------------------------------------------------------

shinyApp(ui, server)
