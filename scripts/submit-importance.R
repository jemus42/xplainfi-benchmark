source(here::here("config.R"))
library(batchtools)
library(dplyr, warn.conflicts = FALSE)
reg = loadRegistry(conf$reg_path, writeable = TRUE)
tab = unwrap(getJobPars(findExperiments(repls = 1:25)))

repls = getJobTable(tab$job.ids)[, .(job.id, repl)]
tab = ijoin(repls, tab)
tab = tab[
	(is.na(n_permutations) | n_permutations <= 100) & (is.na(sage_n_samples) | sage_n_samples <= 100),
]

togo = ijoin(findNotDone(), tab) |> ajoin(findRunning())

res_short = list(walltime = 24 * 3600, memory = 4 * 1024, qos = "medium")
res_long = list(walltime = 7 * 24 * 3600, memory = 4 * 1024, qos = "long")

togo_py = ijoin(togo, findTagged("python"))
togo_r = togo |> ajoin(togo_py)

if (fs::file_exists(here::here("mem-importance.rds"))) {
	estm = readRDS(here::here("mem-importance.rds"))

	togo = ijoin(togo, estm$memory[, .(job.id, memory)])
	data.table::setnames(togo, "memory", "memory_est")
}

if (fs::file_exists(here::here("eta-importance.rds"))) {
	est = readRDS(here::here("eta-importance.rds"))
	togo = ijoin(togo, est$runtimes)

	togo_py = ijoin(togo_py, est$runtimes)
	togo_r = ijoin(togo_r, est$runtimes)

	togo_py_short = togo_py[runtime < 20 * 3600]
	togo_r_short = togo_r[runtime < 20 * 3600]

	togo_py_long = togo_py[runtime > 20 * 3600]
	togo_r_long = togo_r[runtime > 20 * 3600]
}
