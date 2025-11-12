# Package dependencies, will be checked for installation
local({
	packages <- c(
		"xplainfi",
		"mlr3",
		"mlr3learners",
		"mlr3pipelines",
		"mlr3fselect",
		"mlr3torch",
		"reticulate",
		"batchtools",
		"mlbench",
		"mlr3data",
		"batchtools",
		"data.table",
		"checkmate",
		"digest",
		"iml",
		"vip",
		"ranger",
		"nnet",
		"xgboost",
		"arf",
		"partykit",
		"mvtnorm",
		"fs"
	)

	missing_pks <- setdiff(packages, rownames(installed.packages()))

	if (length(missing_pks) > 0) {
		cli::cli_warn(c(
			"!" = "Not all required packages are installed, missing {.val {missing_pks}}",
			i = "Run {.code rv sync} in the terminal to sync dependencies.",
			"See README.md"
		))
	}
})

if (requireNamespace("torch", quietly = TRUE)) {
	if (!torch::torch_is_installed()) {
		cli::cli_warn(c(
			"!" = "torch is not installed yet",
			i = "Run {.code library(mlr3torch)} and follow the instructions on screen"
		))
	}
}

fs::dir_create(here::here("registries", c("runtime", "importance")))

library(batchtools)
library(mlr3)
library(data.table)
