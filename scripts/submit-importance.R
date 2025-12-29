source(here::here("config.R"))
library(batchtools)
library(dplyr, warn.conflicts = FALSE)
reg = loadRegistry(conf$reg_path, writeable = TRUE)
tab = unwrap(getJobPars(findExperiments(repls = 1:25)))

repls = getJobTable(tab$job.ids)[, .(job.id, repl)]
tab = ijoin(repls, tab)
tab = tab[n_permutations <= 100 & sage_n_samples <= 100, ]

est = readRDS(here::here("runtime.rds"))
tab = ijoin(tab, est$runtimes)
