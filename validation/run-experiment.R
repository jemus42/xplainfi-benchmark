# Prepare and submit the validation experiment.
#
# Designed to be run interactively: source down to `report_groups()`, eyeball
# the plan, then run `submit_groups()`. Sourcing the whole file submits.
#
# Cluster configuration comes from batchtools.conf.R -- do not override
# reg$cluster.functions here. QoS is derived from walltime by the Slurm template.
#
# Typical staged workflow (resource estimates come from *completed* jobs):
#   1. Pilot one replication to measure real runtime/memory:
#        XPLAINFI_BENCH_REPLS=1 Rscript validation/run-experiment.R
#   2. When it finishes, run validation/eta.R to write eta-validation.rds.
#   3. Submit the rest, now with estimates: rerun without XPLAINFI_BENCH_REPLS.
library(batchtools)
source(here::here("validation", "config.R"))
source(here::here("setup-common.R")) # pkg check + all R/ helpers via source_r()

require_registry(conf$reg_path)
reg <- loadRegistry(conf$reg_path, writeable = TRUE)
getStatus()

# Everything outstanding and not already in flight (picks up failed/expired too).
# todo() is defined in R/submit-helpers.R.
#
# XPLAINFI_BENCH_REPLS restricts to specific replications ("1", or "1,2"). Use it
# for the pilot pass: replication 1 covers every design cell exactly once, which
# is the coverage estimateRuntimes() needs before eta.R can model anything.
# Unset submits everything outstanding.
ids <- todo_repls()

# Estimates from completed jobs (eta-validation.rds from eta.R; mem-validation.rds
# from the external slurm-memcheck utility, if materialised). Absent on the pilot
# pass -> chunk by job count / default memory.
est <- read_estimates("validation", reg_path = conf$reg_path)

# Double the memory of any expired (OOM/walltime-killed) job being resubmitted, so
# it does not just fail the same way. A group requests the max of its members, so a
# bumped job lifts its whole chunk (safe over-provisioning).
# Pretesting (XPLAINFI_BENCH_REPLS set) submits with deliberately conservative
# resources: small chunks so one OOM cannot take down hundreds of jobs, the
# longest walltime, generous memory, and no reliance on estimates that do not
# exist yet. Measuring cost is the point; economising comes after.
pilot <- nzchar(Sys.getenv("XPLAINFI_BENCH_REPLS"))

groups <- plan_submission(
	ids = ids,
	pilot = pilot,
	runtimes = est$runtimes,
	memory = escalate_memory(base = est$memory)
)
report_groups(groups)

# Inspect `groups` above, then submit (comment out to prepare-only):
submit_groups(groups)
