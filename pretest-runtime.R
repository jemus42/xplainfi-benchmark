# Submit jobs
library(batchtools)
source("config-runtime.R")
cli::cli_alert_info("Loading registry at {.file {fs::path_rel(conf$reg_path)}}")
reg <- loadRegistry(conf$reg_path, writeable = TRUE)
tab <- unwrap(getJobTable())

tab[, .N, by = learner_type]
tab[, .N, by = problem]
tab[, .N, by = algorithm]
tab[, .N, by = .(problem, n_samples, n_features)]
tab[, .N, by = .(algorithm, sampler)]
tab[, .N, by = .(algorithm, n_permutations)]
tab[, .N, by = .(algorithm, n_repeats)]


ids1 = tab[
	repl == 1 &
		n_samples == 100,
	.SD[sample(nrow(.SD), 1)],
	by = c("algorithm", "learner_type", "sampler")
]


ids1 |>
	findNotSubmitted() |>
	submitJobs()


ids = tab[
	repl == 1,
	.SD[sample(nrow(.SD), 1)],
	by = c("algorithm", "learner_type", "sampler", "n_permutations", "early_stopping")
]
