# Script to run the experiment
library(batchtools)

# Load registry
source("config.R")
reg <- loadRegistry(conf$reg_path, writeable = TRUE)
tab = unwrap(getJobTable())
tab[, chunk := sample(job.id)]
getStatus()

reg$cluster.functions = makeClusterFunctionsSSH(
	list(Worker$new("localhost", ncpus = 10, max.load = 40)),
	fs.latency = 0
)

ids = tab[repl <= 10, .SD[sample(nrow(.SD), 1)], by = c("algorithm", "problem")]
ids = tab[repl <= 20, .SD[sample(nrow(.SD), 1)], by = c("algorithm", "problem")]

bikesh = tab[problem == "bike_sharing"]


ijoin(ids, findNotSubmitted()) |>
	ajoin(bikesh) |>
	submitJobs()
