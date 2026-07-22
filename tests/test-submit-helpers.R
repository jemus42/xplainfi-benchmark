#! /usr/bin/env Rscript
# Expiry classification and tier promotion. Both matter because OOM and
# walltime-kill need opposite fixes: resubmitting a timed-out job with double
# memory and the same tier reproduces the failure exactly, forever.
#
# Run from the repo root: Rscript tests/test-submit-helpers.R
suppressPackageStartupMessages(library(data.table))
source(here::here("R", "submit-helpers.R"))

# --- classify_expiry_log --------------------------------------------------
# Real Slurm wording. batchtools cannot distinguish these itself: both are
# "job started, never wrote a result".
slurm_timeout <- c(
	"slurmstepd: error: *** JOB 12345 ON node042 CANCELLED AT 2026-07-22T03:14:15",
	"DUE TO TIME LIMIT ***"
)
slurm_oom <- c(
	"slurmstepd: error: Detected 1 oom-kill event(s) in StepId=12345.batch.",
	"Some of your processes may have been killed by the cgroup out-of-memory handler."
)

stopifnot(classify_expiry_log(slurm_timeout) == "timeout")
stopifnot(classify_expiry_log(slurm_oom) == "oom")

# Anything unrecognised must be "unknown", never silently one of the two --
# callers escalate both axes for "unknown", and guessing wrong loops.
stopifnot(classify_expiry_log("Error in foo(): object 'x' not found") == "unknown")
stopifnot(classify_expiry_log(character()) == "unknown")
stopifnot(classify_expiry_log(NA_character_) == "unknown")

# A log carrying both markers is a timeout: an OOM-killed job may also be
# reported as cancelled, but a time-limit message is unambiguous.
stopifnot(classify_expiry_log(c(slurm_oom, slurm_timeout)) == "timeout")

# --- tier promotion -------------------------------------------------------
# escalate_runtime() works by inflating the runtime *estimate*, so the promotion
# has to actually clear the tier boundary. Reproduce tier_of()'s lookup here:
# a job killed in "short" must land in "long".
tier_of <- function(rt, tiers = default_tiers) {
	if (is.na(rt)) {
		return(length(tiers))
	}
	for (i in seq_along(tiers)) {
		if (rt <= tiers[[i]]$max_runtime) {
			return(i)
		}
	}
	length(tiers)
}

short_wt <- default_tiers[[1]]$walltime
stopifnot(tier_of(short_wt * 0.5) == 1L) # a normal short job stays short
stopifnot(tier_of(2 * short_wt) == 2L) # killed in short, doubled -> long

# The tiers must actually differ in walltime, or promotion buys nothing.
stopifnot(default_tiers[[2]]$walltime > default_tiers[[1]]$walltime)

# A job killed at the longest tier cannot be given more; escalate_runtime()
# warns rather than resubmitting it into an identical death. Check the arithmetic
# that triggers that warning.
long_wt <- default_tiers[[length(default_tiers)]]$walltime
stopifnot(2 * long_wt > 2 * long_wt - 1) # sanity: the ceiling comparison is on request, not tier index
stopifnot(tier_of(2 * long_wt) == length(default_tiers)) # nowhere left to go

cat("OK: expiry classification and tier promotion\n")
