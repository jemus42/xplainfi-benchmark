library(batchtools)
library(dplyr, warn.conflicts = FALSE)

source("config-runtime.R")
reg = loadRegistry(conf$reg_path, writeable = TRUE)

tab = unwrap(getJobPars(findExperiments(repls = 1:25)))
tab = ijoin(tab, getJobTable(tab)[, .(job.id, repl)])

subtab = tab[
  repl <= 10 & 
  n_samples < 10000 &
  (sage_n_samples < 200 | n_repeats < 100) &
  n_features < 50
 ]
togo = ijoin(subtab, findNotDone()) |> ajoin(findRunning()) |> ajoin(findQueued())

est = readRDS("runtime.rds")
togo = ijoin(togo, est$runtimes)

res_short = list(walltime = 24 * 3600, memory = 4 * 1024, partition = "serial_std,serial_long", max.concurrent.jobs = 190)
res_long = list(walltime = 7 * 24 * 3600, memory = 4 * 1024, partition = "serial_long,serial_std", max.concurrent.jobs = 190)

togo_py = ijoin(togo, findTagged("python"))
togo_r = togo |> ajoin(togo_py)

togo_py_short = togo_py[runtime < 20 * 3600]
togo_r_short = togo_r[runtime < 20 * 3600]

togo_py_long = togo_py[runtime > 20 * 3600]
togo_r_long = togo_r[runtime > 20 * 3600]
