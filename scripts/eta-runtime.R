#! /usr/bin/env Rscript

source(here::here("config-runtime.R"))
library(batchtools)
reg = suppressMessages(loadRegistry(conf$reg_path, writeable = FALSE))

tab = unwrap(getJobPars(findExperiments(repls = 1:25)))
tab = tab[n_samples < 10000 & n_features < 50 & (sage_n_samples < 200 | n_repeats < 100)]

cli::cli_h1("Current status for 25 replications and selected parameter configurations")
getStatus(tab)

cli::cli_h1("Current ETA assuming 96 parallel jobs")
est = estimateRuntimes(tab, num.trees = 1000, min.node.size = 5, mtry = 8, max.depth = 10)
print(est, n = 96)

cli::cli_h2("Based on this model")
est$model

saveRDS(est, here::here("runtime.rds"))
