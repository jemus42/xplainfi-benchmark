# Submit jobs
library(batchtools)
reg <- loadRegistry("registry", writeable = TRUE)
tab <- unwrap(getJobTable())

tab[, .N, by = learner_type]
tab[, .N, by = problem]
tab[, .N, by = algorithm]
tab[, .N, by = .(problem, n_samples, n_features)]
tab[, .N, by = .(learner_type, n_trees)]
tab[, .N, by = .(algorithm, reference_proportion)]
tab[, .N, by = .(algorithm, n_refits)]


# Create random subset of jobs for testing
ids = tab[
  learner_type == "featureless",
  .SD[sample(nrow(.SD), 1)],
  by = c("algorithm")
]
submitJobs(findNotSubmitted(ids))

ids = tab[,
  .SD[sample(nrow(.SD), 10)],
  by = c("algorithm", "problem", "learner_type")
]

ids[, chunk := chunk(job.id, chunk.size = 30)]
ids[, .N, by = chunk]

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
