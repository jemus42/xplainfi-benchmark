# Package dependencies, will be checked for installation
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
	# "nnet",
	"xgboost",
	"arf",
	"partykit",
	"mvtnorm",
	"fs"
)

local({
	missing_pks <- all(sapply(
		packages,
		requireNamespace,
		quietly = TRUE
	))
	if (!missing_pks) {
		cli::cli_warn(c(
			"!" = "Not all required packages are installed",
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


# Registry setup
if (!dir.exists(here::here("registries"))) {
	fs::dir_create(here::here("registries", c("runtime", "importance")))
}

library(batchtools)
library(mlr3)
library(data.table)
