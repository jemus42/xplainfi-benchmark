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
#        ids <- todo(repls = 1)
#   2. When it finishes, run runtime/eta.R to write eta-runtime.rds.
#   3. Submit the rest, now with estimates (the default `ids` below).
library(batchtools)
source(here::here("runtime", "config.R"))
source(here::here("setup-common.R")) # pkg check + all R/ helpers via source_r()

reg <- loadRegistry(conf$reg_path, writeable = TRUE)
getStatus()

# Everything outstanding and not already in flight (picks up failed/expired too).
# todo() is defined in R/submit-helpers.R.
ids <- todo()
# Pilot first pass instead:  ids <- todo(repls = 1)

# Runtime estimates from completed jobs (eta-runtime.rds from eta.R). Falls back
# to the legacy tracked snapshot results/runtime-est.rds, then to job-count
# chunking if neither exists.
est <- read_estimates(
	"runtime",
	runtime_path = if (fs::file_exists(here::here("eta-runtime.rds"))) {
		here::here("eta-runtime.rds")
	} else {
		here::here("results", "runtime-est.rds")
	}
)

# Double the memory of any expired (OOM/walltime-killed) job being resubmitted.
groups <- plan_submission(
	ids = ids,
	runtimes = est$runtimes,
	memory = escalate_memory()
)
report_groups(groups)

# Inspect `groups` above, then submit (comment out to prepare-only):
submit_groups(groups)
