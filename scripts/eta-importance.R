#! /usr/bin/env Rscript

source(here::here("config.R"))
library(batchtools)
reg = suppressMessages(loadRegistry(conf$reg_path, writeable = FALSE))

tab = unwrap(getJobPars(findExperiments(repls = 1:25)))
tab = tab[(is.na(n_permutations) | n_permutations <= 100) & (is.na(sage_n_samples) | sage_n_samples <= 100), ]


cli::cli_h1("Current status for 25 replications")
getStatus(tab)

est = estimateRuntimes(tab, num.trees = 1000, min.node.size = 10, mtry = 10, max.depth = 9)

cli::cli_h1("Current ETA assuming 200 parallel jobs")
print(est, n = 200)
cli::cli_inform("Model R^2: {round(est$model$r.squared, 2)}")

saveRDS(est, here::here("runtime.rds"))
