# Prepare and submit the runtime experiment.
#
# Designed to be run interactively: source down to `report_groups()`, eyeball
# the plan, then run `submit_groups()`. Sourcing the whole file submits.
#
# Cluster configuration comes from batchtools.conf.R. QoS is derived from
# walltime by the Slurm template. Backend isolation matters more here than in
# the importance lane: a session shared between backends would bias the timings.
#
# Typical staged workflow (resource estimates come from *completed* jobs):
#   1. Pilot one replication to measure real runtime:
#        ids <- ijoin(findExperiments(repls = 1), todo())
#   2. When it finishes, run runtime/eta.R to refresh results/runtime-est.rds.
#   3. Submit the rest, now with estimates (the default `ids` below).
library(batchtools)
source(here::here("runtime", "config.R"))
source(here::here("R/submit-helpers.R"))

reg <- loadRegistry(conf$reg_path, writeable = TRUE)
getStatus()

# Everything outstanding and not already in flight (picks up failed/expired too).
todo <- function() {
	findNotDone() |> ajoin(findRunning()) |> ajoin(findQueued())
}
ids <- todo()
# Pilot first pass instead:  ids <- ijoin(findExperiments(repls = 1), todo())

# Runtime estimates from completed jobs (written by eta.R); absent on the pilot
# pass, in which case chunking falls back to job count.
est_file <- here::here("results", "runtime-est.rds")
runtimes <- if (fs::file_exists(est_file)) {
	data.table::as.data.table(readRDS(est_file))
} else {
	NULL
}

groups <- plan_submission(
	ids = ids,
	python = findTagged("python"),
	runtimes = runtimes
)
report_groups(groups)

# Inspect `groups` above, then submit (comment out to prepare-only):
submit_groups(groups)
