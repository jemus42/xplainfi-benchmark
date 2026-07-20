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


fs::dir_create(here::here("registries", c("runtime", "importance")))

library(batchtools)
library(mlr3)
library(data.table)

# Source every .R file in R/ into the global environment, like targets::tar_source().
# All R/ files are pure definitions (functions + a couple of constants) with no
# load-order dependencies, so a blanket source is safe. Call standalone to reload.
source_r <- function(dir = here::here("R")) {
	files <- list.files(dir, pattern = "\\.R$", full.names = TRUE)
	invisible(lapply(files, source))
}
source_r()
