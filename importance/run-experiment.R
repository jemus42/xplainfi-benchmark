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
#   2. When it finishes, run importance/eta.R to write eta-/mem-importance.rds.
#   3. Submit the rest, now with estimates (the default `ids` below).
library(batchtools)
source(here::here("importance", "config.R"))
source(here::here("R/submit-helpers.R"))

reg <- loadRegistry(conf$reg_path, writeable = TRUE)
getStatus()

# Everything outstanding and not already in flight (picks up failed/expired too).
todo <- function() {
	findNotDone() |> ajoin(findRunning()) |> ajoin(findQueued())
}
ids <- todo()
# Pilot first pass instead:  ids <- ijoin(findExperiments(repls = 1), todo())

# Estimates from completed jobs (written by eta.R); absent on the pilot pass, in
# which case chunking falls back to job count / default memory.
read_est <- function(path, field) {
	if (fs::file_exists(path)) data.table::as.data.table(readRDS(path)[[field]]) else NULL
}
runtimes <- read_est(here::here("eta-importance.rds"), "runtimes")
memory <- read_est(here::here("mem-importance.rds"), "memory")

groups <- plan_submission(
	ids = ids,
	python = findTagged("python"),
	runtimes = runtimes,
	memory = memory
)
report_groups(groups)

# Inspect `groups` above, then submit (comment out to prepare-only):
submit_groups(groups)
