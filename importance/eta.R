#! /usr/bin/env Rscript
# Estimate runtimes from completed jobs -> eta-importance.rds, which
# run-experiment.R uses to chunk the next submission. See R/estimate.R.
#
# Memory now comes from the registry itself: `measure.memory = TRUE` in
# batchtools.conf.R makes batchtools record mem.used per finished job, so
# write_memory_estimates() below writes mem-importance.rds from observation. That
# replaces the external slurm-memcheck route; plan_submission() reads both files
# via read_estimates() and falls back to mem_default when either is missing.
library(batchtools)
source(here::here("importance", "config.R"))
source(here::here("setup-common.R")) # pkg check + all R/ helpers via source_r()

write_estimates(
	conf$reg_path,
	prefix = "importance",
	rf = list(num.trees = 1000, min.node.size = 10, mtry = 10, max.depth = 9)
)

# Memory sizing from the registry's own mem.used measurements. Kept separate from
# the runtime model above because it needs no fitting -- see R/estimate.R.
write_memory_estimates(conf$reg_path, prefix = "importance")
