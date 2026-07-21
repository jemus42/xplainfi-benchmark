#! /usr/bin/env Rscript
# Estimate runtimes from completed jobs -> eta-importance.rds, which
# run-experiment.R uses to chunk the next submission. See R/estimate.R.
#
# Memory is not modelled here (that batchtools experiment was dropped); on the
# BIPS cluster it comes from the external `slurm-memcheck` utility. Materialise its
# output as mem-importance.rds to have plan_submission size memory requests --
# otherwise it uses a default.
library(batchtools)
source(here::here("importance", "config.R"))
source(here::here("setup-common.R")) # pkg check + all R/ helpers via source_r()

write_estimates(
	conf$reg_path,
	prefix = "importance",
	rf = list(num.trees = 1000, min.node.size = 10, mtry = 10, max.depth = 9)
)
