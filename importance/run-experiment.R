# Prepare and submit the importance experiment.
#
# Designed to be run interactively: source down to `report_groups()`, eyeball
# the plan, then run `submit_groups()`. Sourcing the whole file submits.
#
# Cluster configuration comes from batchtools.conf.R -- do not override
# reg$cluster.functions here. QoS is derived from walltime by the Slurm template.
#
# Typical staged workflow (resource estimates come from *completed* jobs):
#   1. Pilot one replication to measure real runtime/memory:
#        ids <- ijoin(findExperiments(repls = 1), todo())
#   2. When it finishes, run importance/eta.R to write eta-importance.rds.
#   3. Submit the rest, now with estimates (the default `ids` below).
library(batchtools)
source(here::here("importance", "config.R"))
source(here::here("setup-common.R")) # pkg check + all R/ helpers via source_r()

reg <- loadRegistry(conf$reg_path, writeable = TRUE)
getStatus()

# Everything outstanding and not already in flight (picks up failed/expired too).
# todo() is defined in R/submit-helpers.R.
ids <- todo()
# Pilot first pass instead:  ids <- ijoin(findExperiments(repls = 1), todo())

# Estimates from completed jobs (eta-importance.rds from eta.R; mem-importance.rds
# from the external slurm-memcheck utility, if materialised). Absent on the pilot
# pass -> chunk by job count / default memory.
est <- read_estimates("importance")

groups <- plan_submission(
	ids = ids,
	runtimes = est$runtimes,
	memory = est$memory
)
report_groups(groups)

# Inspect `groups` above, then submit (comment out to prepare-only):
submit_groups(groups)
