# Submit jobs
library(batchtools)
reg <- loadRegistry("registry", writeable = TRUE)
tab <- unwrap(getJobTable())

tab[, .N, by = learner_type]
tab[, .N, by = problem]
tab[, .N, by = algorithm]


# Create random subset of jobs for testing

ids = tab[,
  .SD[sample(nrow(.SD), 1)],
  by = c("algorithm")
]
submitJobs(findNotSubmitted(ids))

ids = tab[
  learner_type == "featureless",
  .SD[sample(nrow(.SD), 2)],
  by = c("algorithm")
]
submitJobs(findNotSubmitted(ids))

ids = tab[
  learner_type == "featureless",
  .SD[sample(nrow(.SD), 2)],
  by = c("learner_type")
]
submitJobs(findNotSubmitted(ids))


ids = tab[,
  .SD[sample(nrow(.SD), 1)],
  by = c("algorithm", "problem", "learner_type")
]
submitJobs(findNotSubmitted(ids))
ijoin(getErrorMessages(), tab)

getStatus()
