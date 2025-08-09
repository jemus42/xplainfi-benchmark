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
  .SD[sample(nrow(.SD), 1)],
  by = c("algorithm", "problem", "learner_type")
]
submitJobs(findNotSubmitted(ids))

findTagged("runtime") |>
  findNotSubmitted() 
