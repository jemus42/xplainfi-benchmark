# Simple Shiny app for exploring runtime benchmark results
# Run with: shiny::runApp("R/shiny-runtime.R")

library(shiny)
library(data.table)
library(ggplot2)
library(bslib)

# Load data and helper functions
source(here::here("R", "analysis.R"))

file_runtime <- fs::path(here::here("results", "runtime"), "runtime", ext = "rds")
if (!fs::file_exists(file_runtime)) {
	stop("Run analysis-runtime.R first to generate runtime.rds")
}
runtimes <- readRDS(file_runtime)

# UI -------------------------------------------------------------------------

ui <- page_sidebar(
	title = "xplainfi Runtime Benchmark Explorer",
	theme = bs_theme(bootswatch = "flatly"),

	sidebar = sidebar(
		width = 300,

		selectInput(
			"method",
			"Method",
			choices = levels(runtimes$method),
			selected = "PFI",
			multiple = TRUE
		),

		selectInput(
			"package",
			"Package",
			choices = levels(runtimes$package),
			selected = levels(runtimes$package),
			multiple = TRUE
		),

		selectInput(
			"learner_type",
			"Learner Type",
			choices = levels(runtimes$learner_type),
			selected = "linear",
			multiple = TRUE
		),

		conditionalPanel(
			condition = "input.method.includes('CFI') || input.method.includes('cSAGE')",
			selectInput(
				"sampler",
				"Sampler (conditional methods)",
				choices = c("all", na.omit(unique(as.character(runtimes$sampler)))),
				selected = "all",
				multiple = FALSE
			)
		),

		conditionalPanel(
			condition = "input.method.includes('mSAGE') || input.method.includes('cSAGE')",
			selectInput(
				"n_permutations",
				"n_permutations (SAGE)",
				choices = c("all", sort(unique(na.omit(runtimes$n_permutations)))),
				selected = "all",
				multiple = FALSE
			),
			selectInput(
				"sage_n_samples",
				"sage_n_samples (SAGE)",
				choices = c("all", sort(unique(na.omit(runtimes$sage_n_samples)))),
				selected = "all",
				multiple = FALSE
			)
		),

		hr(),

		selectInput(
			"n_samples",
			"n_samples",
			choices = c("all", sort(unique(runtimes$n_samples))),
			selected = "all",
			multiple = FALSE
		),

		selectInput(
			"n_features",
			"n_features",
			choices = c("all", sort(unique(runtimes$n_features))),
			selected = "all",
			multiple = FALSE
		),

		hr(),

		selectInput(
			"color_by",
			"Color by",
			choices = c(
				"package",
				"method",
				"learner_type",
				"sampler",
				"n_samples",
				"n_features",
				"n_permutations",
				"sage_n_samples"
			),
			selected = "package"
		),

		selectInput(
			"facet_by",
			"Facet by",
			choices = c(
				"method",
				"package",
				"learner_type",
				"sampler",
				"n_samples",
				"n_features",
				"n_permutations",
				"sage_n_samples"
			),
			selected = c("n_samples", "n_features"),
			multiple = TRUE
		),

		radioButtons(
			"scale_type",
			"Scale",
			choices = c("seconds", "log10 seconds", "relative"),
			selected = "log10 seconds",
			inline = TRUE
		),

		checkboxInput("hide_legend", "Hide legend", value = FALSE),

		hr(),

		actionButton("update_plot", "Update Plot", class = "btn-primary"),

		hr(),

		h6("Data summary"),
		verbatimTextOutput("data_summary")
	),

	card(
		card_header("Runtime Plot"),
		plotOutput("runtime_plot", height = "800px")
	),

	card(
		card_header("Runtime Summary"),
		verbatimTextOutput("runtime_summary")
	)
)

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
	# Update available packages based on selected method
	observeEvent(input$method, {
		available_pkgs <- as.character(unique(runtimes[method %in% input$method, package]))
		# Keep currently selected packages that are still available, or select all available
		current_selection <- input$package
		new_selection <- intersect(current_selection, available_pkgs)
		if (length(new_selection) == 0) {
			new_selection <- available_pkgs
		}
		updateSelectInput(
			session,
			"package",
			choices = available_pkgs,
			selected = new_selection
		)
	})

	# Reactive filtered data
	filtered_data <- reactive({
		req(input$method, input$package, input$learner_type)

		data <- runtimes[
			method %in% input$method & package %in% input$package & learner_type %in% input$learner_type
		]

		# Filter by sampler for conditional methods
		if (!is.null(input$sampler) && input$sampler != "all") {
			data <- data[is.na(sampler) | sampler == input$sampler]
		}

		# Filter by n_permutations for SAGE methods
		if (!is.null(input$n_permutations) && input$n_permutations != "all") {
			data <- data[is.na(n_permutations) | n_permutations == as.integer(input$n_permutations)]
		}

		# Filter by sage_n_samples for SAGE methods
		if (!is.null(input$sage_n_samples) && input$sage_n_samples != "all") {
			data <- data[is.na(sage_n_samples) | sage_n_samples == as.integer(input$sage_n_samples)]
		}

		# Filter by n_samples
		if (input$n_samples != "all") {
			data <- data[n_samples == as.integer(input$n_samples)]
		}

		# Filter by n_features
		if (input$n_features != "all") {
			data <- data[n_features == as.integer(input$n_features)]
		}

		data
	})

	# Data summary
	output$data_summary <- renderPrint({
		data <- filtered_data()
		cat(sprintf("Observations: %d\n", nrow(data)))
		cat(sprintf("Jobs: %d\n", uniqueN(data$job.id)))
		cat(sprintf("Methods: %s\n", paste(unique(data$method), collapse = ", ")))
		cat(sprintf("Packages: %s\n", paste(unique(data$package), collapse = ", ")))
		cat(sprintf("Learners: %s\n", paste(unique(data$learner_type), collapse = ", ")))
		cat(sprintf("n_samples: %s\n", paste(sort(unique(data$n_samples)), collapse = ", ")))
		cat(sprintf("n_features: %s\n", paste(sort(unique(data$n_features)), collapse = ", ")))
	})

	# Runtime summary table
	output$runtime_summary <- renderPrint({
		data <- filtered_data()

		if (nrow(data) == 0) {
			cat("No data for selection\n")
			return()
		}

		summary_dt <- data[,
			.(
				median_s = round(median(runtime), 2),
				q25_s = round(quantile(runtime, 0.25), 2),
				q75_s = round(quantile(runtime, 0.75), 2),
				n = .N
			),
			by = .(method, package, learner_type)
		]

		summary_dt <- summary_dt[order(median_s)]

		cat("Runtime summary (seconds):\n\n")
		print(summary_dt, row.names = FALSE)
	})

	# Main plot
	output$runtime_plot <- renderPlot({
		input$update_plot # Trigger on button click

		isolate({
			data <- filtered_data()

			if (nrow(data) == 0) {
				ggplot() +
					annotate("text", x = 0.5, y = 0.5, label = "No data for selection", size = 6) +
					theme_void()
			} else {
				data <- copy(data)

				# Compute relative runtime if selected
				plot_var <- "runtime"
				x_lab <- "Runtime (seconds)"

				if (input$scale_type == "relative") {
					# Group by all relevant parameters to match xplainfi baseline
					group_cols <- c(
						"method",
						"learner_type",
						"n_samples",
						"n_features",
						"sampler",
						"n_permutations",
						"sage_n_samples"
					)
					# Only use columns that exist and have non-NA values
					group_cols <- intersect(group_cols, names(data))

					# Compute median runtime for xplainfi per group
					baseline <- data[
						package == "xplainfi",
						.(baseline_runtime = median(runtime, na.rm = TRUE)),
						by = group_cols
					]

					# Join baseline and compute relative runtime
					data <- merge(data, baseline, by = group_cols, all.x = TRUE)
					data[, runtime_relative := runtime / baseline_runtime]

					plot_var <- "runtime_relative"
					x_lab <- "Runtime (relative to xplainfi)"
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
						fill = .data[[input$color_by]],
						color = .data[[input$color_by]]
					)
				) +
					geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
					labs(
						title = "Runtime Comparison",
						x = x_lab,
						y = NULL,
						fill = input$color_by,
						color = input$color_by
					) +
					theme_minimal(base_size = 16) +
					theme(
						legend.position = if (input$hide_legend) "none" else "top",
						plot.title.position = "plot"
					)

				# Color palette
				if (input$color_by == "package") {
					p <- p + scale_fill_manual(values = pal_package, aesthetics = c("fill", "color"))
				} else {
					p <- p + scale_fill_brewer(palette = "Dark2", aesthetics = c("fill", "color"))
				}

				# Add faceting
				facet_vars <- input$facet_by
				if (length(facet_vars) >= 1) {
					p <- p + facet_wrap(facets = facet_vars, ncol = 2, labeller = label_both)
				}

				# X-axis scaling with pretty labels
				if (input$scale_type == "log10 seconds") {
					p <- p + scale_x_log10(labels = scales::label_number())
				} else if (input$scale_type == "relative") {
					# Add reference line at 1 (xplainfi baseline)
					p <- p +
						geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
						scale_x_continuous(labels = scales::label_number(suffix = "x"))
				} else {
					p <- p + scale_x_continuous(labels = scales::label_number())
				}

				p
			}
		})
	})
}

# Run app --------------------------------------------------------------------

shinyApp(ui, server)
