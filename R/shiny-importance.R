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
			choices = c("learner_type", "method", "package"),
			selected = "learner_type",
			multiple = TRUE
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

	# Update facet/color options based on problem (add correlation for correlated problem)
	observeEvent(input$problem, {
		base_choices <- c("learner_type", "method", "package")
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
		updateSelectInput(
			session,
			"color_by",
			choices = color_choices,
			selected = if (input$color_by %in% color_choices) input$color_by else "package"
		)
	})

	# Reactive filtered data
	filtered_data <- reactive({
		req(input$problem, input$method, input$package, input$learner_type)

		data <- importances[
			problem == input$problem &
				method %in% input$method &
				package %in% input$package &
				learner_type %in% input$learner_type
		]

		# Filter by correlation for correlated problem
		# Skip filter if correlation is used for faceting or coloring (need all values)
		correlation_in_viz <- "correlation" %in% c(input$facet_by, input$color_by)
		if (input$problem == "correlated" && input$correlation != "both" && !correlation_in_viz) {
			data <- data[correlation == as.numeric(input$correlation)]
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
	})

	# Quality info
	output$quality_info <- renderPrint({
		data <- filtered_data()

		# Learner performance
		perf <- data[, .(mean_perf = mean(learner_performance, na.rm = TRUE)), by = learner_type]
		cat("Learner Performance (R²):\n")
		for (i in seq_len(nrow(perf))) {
			status <- if (perf$mean_perf[i] < 0.5) " ⚠ LOW" else ""
			cat(sprintf("  %s: %.3f%s\n", perf$learner_type[i], perf$mean_perf[i], status))
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
