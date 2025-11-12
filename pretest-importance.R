# Submit jobs
library(batchtools)
source("config.R")
cli::cli_alert_info("Loading registry at {.file {fs::path_rel(conf$reg_path)}}")
reg <- loadRegistry(conf$reg_path, writeable = TRUE)
tab <- unwrap(getJobTable())

tab[, .N, by = learner_type]
tab[, .N, by = problem]
tab[, .N, by = algorithm]
tab[, .N, by = .(algorithm, sampler)]
tab[, .N, by = .(algorithm, n_permutations)]
tab[, .N, by = .(algorithm, n_repeats)]

# Create random subset of jobs for testing

ids1 = tab[
	repl == 1 &
		(n_samples == 100),
	.SD[sample(nrow(.SD), 1)],
	by = c("algorithm", "problem", "learner_type", "sampler")
]

ids1 |>
	# dplyr::filter(!(algorithm %in% c("LOCO", "PFI", "CFI", "MarginalSAGE", "ConditionalSAGE"))) |>
	findNotSubmitted() |>
	submitJobs()


ids2 = tab[
	repl == 1 &
		problem == "bike_sharing",
	.SD[sample(nrow(.SD), 1)],
	by = c("algorithm", "learner_type", "sampler")
]

ids2 |>
	# dplyr::filter(!(algorithm %in% c("LOCO", "PFI", "CFI", "MarginalSAGE", "ConditionalSAGE"))) |>
	findNotSubmitted() |>
	submitJobs()

# submitJobs(ids1)

ids2 = tab[
	grepl(pattern = "SAGE", x = algorithm),
	.SD[sample(nrow(.SD), 1)],
	by = c("problem", "learner_type", "sampler")
]

# submitJobs(findNotSubmitted(ids))
ids = rbind(ids1, ids2)
ids = unique(ids)

ids[, chunk := chunk(job.id, chunk.size = 30)]

submitJobs(findNotSubmitted(ids))


ids1 = tab[,
	.SD[sample(nrow(.SD), 1)],
	by = c("algorithm", "problem", "learner_type", "sampler", "n_samples")
]
submitJobs(findNotSubmitted(ids1))

ids = tab[
	repl == 1 &
		problem == "bike_sharing",
	.SD[sample(nrow(.SD), 1)],
	by = c("algorithm", "learner_type")
]

ijoin(ids, findTagged("python")) |>
	submitJobs()

submitJobs(findNotSubmitted(ids))
getStatus()

loadResult(findDone()[1])

errs = tab[getErrorMessages()][, .(algorithm, sampler, problem, getErrorMessages())]

errs |> View()
