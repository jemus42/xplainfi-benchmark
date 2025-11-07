# Submit jobs
library(batchtools)
source("config.R")
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

# Create random subset of jobs for testing

ids1 = tab[
	repl == 1 &
		n_samples == 100,
	.SD[sample(nrow(.SD), 1)],
	by = c("algorithm", "problem", "learner_type", "sampler")
]

ids1 |>
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


ijoin(findNotSubmitted(), ids[, .(job.id, chunk)]) |>
	submitJobs()

findTagged("runtime") |>
	ijoin(findExperiments(repls = c(1, 2))) |>
	findNotSubmitted() |>
	submitJobs()

ids = findTagged("runtime") |>
	findNotSubmitted()

ids[, chunk := chunk(job.id, chunk.size = 50)]
submitJobs(ids[, .(job.id, chunk)])


ids = ijoin(tab, findNotSubmitted())
ids[, chunk := chunk(algorithm, chunk.size = 100)]
ids[, .N, by = chunk]
ids = ids[, .(job.id, chunk)]

submitJobs(ids, resources = list(walltime = 12 * 3600))


tab <- unwrap(getJobTable())

tab[algorithm == "PFI_fippy"][1, ] |> testJob()
tab[algorithm == "CFI_fippy"][1, ] |> testJob()
tab[algorithm == "KernelSAGE"][1, ] |> testJob()


testJob(6121)
