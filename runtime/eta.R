#! /usr/bin/env Rscript
# Estimate runtimes from completed jobs -> eta-runtime.rds, which
# run-experiment.R uses to chunk the next submission. See R/estimate.R.
library(batchtools)
source(here::here("runtime", "config.R"))
source(here::here("setup-common.R")) # pkg check + all R/ helpers via source_r()

# Train on the first 20 replications' finished jobs.
write_estimates(
	conf$reg_path,
	prefix = "runtime",
	ids = \(reg) findExperiments(repls = 1:20, reg = reg),
	rf = list(num.trees = 1000, min.node.size = 5, mtry = 8, max.depth = 10),
	n_print = 1500
)
