# Script to run the experiment
library(batchtools)
source("setup-batchtools-runtime.R")

# Load registry
source("config-runtime.R")
reg <- loadRegistry(conf$reg_path, writeable = TRUE)
tab = unwrap(getJobTable())
runtime_est = readRDS("results/runtime-est.rds")
tab = rjoin(tab, runtime_est)
tab[, python := grepl("python", tags)]

tab[, .N, by = c("n_samples", "n_features", "algorithm", "sampler")]

batch1 = tab[repl <= 10]

batch1_py = batch1[(python)]
batch1_r = batch1[!(python)]

batch1_py = batch1[, chunk := binpack(runtime, 12 * 3600)]
batch1_r = batch1[, chunk := binpack(runtime, 12 * 3600)]

batch1_py[, list(runtime = sum(runtime)), by = chunk]
batch1_r[, list(runtime = sum(runtime)), by = chunk]

ijoin(batch1_r, findNotSubmitted()) |>
	submitJobs(resources = list(walltime = 24 * 3600, memory = 3 * 1024))
ijoin(batch1_py, findNotSubmitted()) |>
	submitJobs(batch1_py, resources = list(walltime = 24 * 3600, memory = 3 * 1024))

batch2 = tab[repl > 10 & repl <= 20]
batch2_py = batch2[(python)]
batch2_r = batch2[!(python)]

batch2_py = batch2[, chunk := binpack(runtime, 12 * 3600)]
batch2_r = batch2[, chunk := binpack(runtime, 12 * 3600)]

ijoin(batch2_r, findNotSubmitted()) |>
	submitJobs(resources = list(walltime = 24 * 3600, memory = 3 * 1024))
ijoin(batch2_py, findNotSubmitted()) |>
	submitJobs(batch1_py, resources = list(walltime = 24 * 3600, memory = 3 * 1024))
