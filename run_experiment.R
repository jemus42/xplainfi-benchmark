# Script to run the experiment
library(batchtools)

# Load registry
source("config.R")
reg <- loadRegistry(reg_path, writeable = TRUE)

# Check status
getStatus()
tab = unwrap(getJobTable())

# For testing: submit only a subset
# test_jobs <- findJobs(prob.name = "friedman1", algo.name = "PFI")[1:2]
# submitJobs(test_jobs)

# Submit all jobs
submitJobs(findNotSubmitted())

ids = tab[, .SD[sample(nrow(.SD), 1)], by = c("algorithm", "problem")]
setkeyv(ids, "job.id")
ids[, .(job.id, algorithm, problem)]

tab[, .(job.id)] |>
  findNotSubmitted() |>
  head(3) |>
  submitJobs()


getStatus()
getErrorMessages()
