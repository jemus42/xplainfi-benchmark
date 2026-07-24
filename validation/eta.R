#! /usr/bin/env Rscript
# Estimate runtimes from completed jobs -> eta-validation.rds, which
# run-experiment.R uses to chunk the next submission. See R/estimate.R.
#
# Memory is NOT modelled here. batchtools' own `mem.used` is a gc()-based R-heap
# estimate and is often far off for these methods, so it is not used: on the BIPS
# cluster memory comes from the external `slurm-memcheck` utility (parses sacct).
# Feed its TSV to write_memory_estimates() (R/estimate.R) to write
# mem-validation.rds, which plan_submission uses to size memory requests:
#   slurm-memcheck --since now-1day --tsv > mem.tsv
#   write_memory_estimates("mem.tsv", "validation")   # after resume("validation")
# Without it, plan_submission falls back to mem_default.
library(batchtools)
source(here::here("validation", "config.R"))
source(here::here("setup-common.R")) # pkg check + all R/ helpers via source_r()

write_estimates(
	conf$reg_path,
	prefix = "validation",
	rf = list(num.trees = 1000, min.node.size = 10, mtry = 10, max.depth = 9)
)
