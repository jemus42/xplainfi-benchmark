#! /usr/bin/env Rscript

source(here::here("config.R"))
source(here::here("R/estimateMemory.R"))
library(batchtools)
reg = suppressMessages(loadRegistry(conf$reg_path, writeable = FALSE))

tab = unwrap(getJobPars())

cli::cli_h1("Current status for all replications")
getStatus(tab)

est = estimateRuntimes(tab, num.trees = 1000, min.node.size = 10, mtry = 10, max.depth = 9)

cli::cli_h1("Current ETA assuming 1000 parallel jobs")
print(est, n = 1000)
cli::cli_inform("Model R^2: {round(est$model$r.squared, 2)}")

saveRDS(est, here::here("eta-importance.rds"))

cli::cli_h1("Memory estimation")
mem_est = estimateMemory(tab, num.trees = 1000, min.node.size = 10, mtry = 10, max.depth = 9)
print(mem_est)
cli::cli_inform("Model R^2: {round(mem_est$model$r.squared, 2)}")

saveRDS(mem_est, here::here("mem-importance.rds"))
