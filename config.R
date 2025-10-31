# Configuration file for batchtools experiment

# Package dependencies, will be checked for installation
packages <- c(
	"xplainfi",
	"mlr3",
	"mlr3learners",
	# "mlr3filters",
	"mlr3pipelines",
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
	"arf",
	"partykit",
	"mvtnorm"
)

stopifnot(
	"Not all packages installed" = all(sapply(
		packages,
		requireNamespace,
		quietly = TRUE
	))
)


# Registry setup
if (!dir.exists(here::here("registries"))) {
	dir.create(here::here("registries"))
}

# Experiment settings
conf <- list(
	# General batchtools settings
	reg_path = fs::path(
		here::here("registries"),
		paste0("xplainfi-", packageVersion("xplainfi"))
	),
	seed = 2025,
	repls = 1,
	# Samples to generate or to subsample real data to (bike_sharing)
	n_samples = c(100, 500, 1000),
	# Only one task with variable number of features
	n_features = c(5, 10, 50),
	# Affects correlation task
	correlation = c(0.5, 0.9),
	# Affects PFI, CFI, RFI, and LOCO iterations
	n_repeats = c(1, 10),
	# For SAGE permutations
	n_permutations = c(5, 10, 30),
	# Size of sampled data used for Monte Carlo integration in SAGE methods
	sage_n_samples = 20L,
	# Types of learners to use for each method, uses create_learner helper
	learner_types = c("featureless", "linear", "rf", "mlp"),
	# Fixed number of trees for rf (not varied in experiments)
	n_trees = 500L,
	# Conditional samplers for CFI, RFI, and ConditionalSAGE
	samplers = c(
		"arf",
		"gaussian",
		"knn",
		"ctree"
	)
)
