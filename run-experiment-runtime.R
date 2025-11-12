# Script to run the experiment
library(batchtools)

# Load registry
source("config-runtime.R")
reg <- loadRegistry(conf$reg_path, writeable = TRUE)
tab = unwrap(getJobTable())
runtime_est = readRDS("results/runtime-est.rds")
tab = rjoin(tab, est$runtimes)
tab[, python := grepl("python", tags)]

tab[, .N, by = c("n_samples", "n_features", "algorithm", "sampler")]

# Submit all jobs
submitJobs(findNotSubmitted())

ids = tab[repl <= 10, chunk := binpack(job.id, 30)]
ids

ids = tab[
	repl == 1,
	.SD[sample(nrow(.SD), 1)],
	by = c("n_samples", "n_features", "algorithm", "learner_type", "n_permutations", "sage_n_samples")
]
submitJobs(ids)

ids[n_samples <= 1000][, chunk := chunk(job.id, 30)][] |>
	ijoin(findNotSubmitted()) |>
	submitJobs(
		resources = list(
			walltime = 2 * 3600L
		)
	)

ids[n_samples > 1000][, chunk := chunk(job.id, chunk.size = 2)][] |>
	ijoin(findNotSubmitted()) |>
	submitJobs(
		resources = list(
			walltime = 6 * 3600L
		)
	)


# Load registry
source("config-runtime.R")
reg <- loadRegistry("registries/runtime/xplainfi-0.2.0/", writeable = TRUE)
tab = unwrap(getJobTable())
est = estimateRuntimes(tab)
est$runtimes
print(est, n = 20)

tab = rjoin(tab, est$runtimes)
