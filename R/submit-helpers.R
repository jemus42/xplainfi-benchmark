# Job grouping for submission
#
# Sourced by the `run-experiment.R` scripts only -- not shipped to workers.
#
# Jobs are grouped on two axes, in order:
#
#   1. Backend (R vs Python). batchtools runs every job sharing a `chunk`
#      value sequentially in ONE R session, so R-backed jobs (mlr3torch ->
#      libtorch) and Python-backed jobs (reticulate -> torch) must never share
#      a chunk, or two torch runtimes load into the same process.
#
#   2. Resource tier. A single submitJobs() call carries ONE `resources` list,
#      so the jobs in it must have aligned requirements. Tiers bucket by
#      estimated runtime; each tier maps to a requested walltime. QoS is
#      derived from walltime by the Slurm template, so it is NOT set here.
#      Memory is taken from pretested estimates when available.
#
# The output is a flat list of submission groups; each is one submitJobs() call.

# Jobs outstanding and not already in flight: not-done minus running/queued.
# Re-runnable -- picks up failed/expired jobs without touching in-flight ones.
# Requires a loaded registry (call after loadRegistry / setup-batchtools.R).
# `...` is forwarded to findExperiments() and intersected, to scope the result:
#   todo()                          # everything outstanding
#   todo(repls = 1)                 # pilot: only replication 1
#   todo(prob.name = "friedman1")   # one problem
todo <- function(..., reg = batchtools::getDefaultRegistry()) {
	out <- batchtools::findNotDone(reg = reg) |>
		batchtools::ajoin(batchtools::findRunning(reg = reg)) |>
		batchtools::ajoin(batchtools::findQueued(reg = reg))
	if (...length() > 0L) {
		out <- batchtools::ijoin(out, batchtools::findExperiments(..., reg = reg))
	}
	out
}

# Bump memory for jobs that expired (OOM / walltime kill) so a resubmit does not
# just fail the same way. Reads each expired job's last-REQUESTED memory from the
# registry and multiplies by `factor` (default 2, conservative). The new request
# is recorded on resubmit, so repeated expiries compound automatically -- no
# separate attempt counter needed.
#
# base      optional data.table(job.id, memory[MB]) of estimates for non-expired
#           jobs (e.g. slurm-memcheck output); expired jobs override these.
# factor    memory multiplier for expired jobs
# expired   job ids to bump; defaults to findExpired() (injectable for testing)
#
# Returns data.table(job.id, memory) to pass as plan_submission(memory = ). A job
# whose group in plan_submission contains it will request at least this much
# (groups request the max of their members).
escalate_memory <- function(
	base = NULL,
	factor = 2,
	reg = batchtools::getDefaultRegistry(),
	expired = batchtools::findExpired(reg = reg)
) {
	out <- if (is.null(base)) {
		data.table::data.table(job.id = integer(), memory = numeric())
	} else {
		data.table::as.data.table(base)[, .(job.id, memory)]
	}
	expired <- data.table::as.data.table(expired)
	if (nrow(expired) == 0L) {
		return(out)
	}
	res <- batchtools::getJobResources(expired, reg = reg)
	req <- res[, .(
		job.id,
		memory = factor * vapply(resources, function(r) as.numeric(r$memory %||% NA), numeric(1))
	)]
	# Expired jobs override any base estimate; keep base rows for the rest.
	data.table::rbindlist(
		list(out[!job.id %in% req$job.id], req),
		use.names = TRUE
	)
}

# Ordered runtime ceilings -> requested walltime. A job lands in the first tier
# whose max_runtime it fits under. Edit here to add tiers (e.g. an "xlong").
default_tiers <- list(
	list(name = "short", max_runtime = 20 * 3600, walltime = 24 * 3600),
	list(name = "long", max_runtime = Inf, walltime = 7 * 24 * 3600)
)

# Build submission groups from a set of job ids.
#
# ids            data.table with a `job.id` column (e.g. findNotSubmitted())
# python         job ids using a Python backend; defaults to findTagged("python"),
#                which is always the right set in this benchmark
# runtimes       optional data.table(job.id, runtime[seconds]) from eta.R. Drives
#                both tiering and bin-packing. Jobs with no estimate go to the
#                last (safest) tier and chunk alone.
# memory         optional data.table(job.id, memory[MB]) from a pretest run.
# tiers          see default_tiers
# target_seconds bin-pack chunks up to this wall-clock (< the tier walltime, to
#                leave headroom)
# mem_headroom   multiply the per-group max estimate by this
# mem_default    requested memory (MB) when no estimate is available
# chunk_size     jobs per chunk when no runtimes are given
#
# Returns a list of groups, each: list(backend, tier, resources, jobs), where
# `jobs` is data.table(job.id, chunk) ready for submitJobs(). Groups with no
# jobs are omitted.
plan_submission <- function(
	ids,
	python = batchtools::findTagged("python"),
	runtimes = NULL,
	memory = NULL,
	tiers = default_tiers,
	target_seconds = 12 * 3600,
	mem_headroom = 1.3,
	mem_default = 4 * 1024,
	chunk_size = 20L
) {
	ids <- data.table::as.data.table(ids)[, .(job.id)]
	ids[,
		backend := data.table::fifelse(
			job.id %in% data.table::as.data.table(python)$job.id,
			"python",
			"r"
		)
	]
	if (!is.null(runtimes)) {
		ids <- merge(ids, runtimes[, .(job.id, runtime)], by = "job.id", all.x = TRUE)
	} else {
		ids[, runtime := NA_real_]
	}
	if (!is.null(memory)) {
		ids <- merge(ids, memory[, .(job.id, memory)], by = "job.id", all.x = TRUE)
	} else {
		ids[, memory := NA_real_]
	}

	# Assign each job to the first tier it fits under; NA runtime -> last tier.
	tier_of <- function(rt) {
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
	ids[, tier_i := vapply(runtime, tier_of, integer(1))]

	groups <- list()
	offset <- 0L
	for (be in c("r", "python")) {
		for (ti in seq_along(tiers)) {
			grp <- ids[backend == be & tier_i == ti]
			if (nrow(grp) == 0L) {
				next
			}
			offset <- offset + 1L
			if (is.null(runtimes)) {
				grp[, chunk := batchtools::chunk(job.id, chunk.size = chunk_size)]
			} else {
				# NA runtime -> assume the target so it chunks by itself
				grp[, rt := data.table::fifelse(is.na(runtime), target_seconds, runtime)]
				# Capacity must cover the largest single job (which chunks alone);
				# shorter jobs still pack up to `target_seconds`.
				cap <- max(target_seconds, grp$rt)
				grp[, chunk := batchtools::binpack(rt, chunk.size = cap)]
			}
			grp[, chunk := chunk + offset * 1e6L]

			est <- grp$memory[!is.na(grp$memory)]
			grp_mem <- if (length(est) > 0) {
				max(ceiling(max(est) * mem_headroom), mem_default)
			} else {
				mem_default
			}

			walltime <- tiers[[ti]]$walltime
			if (!is.null(runtimes)) {
				worst <- grp[, sum(rt), by = chunk][["V1"]]
				if (any(worst > walltime)) {
					cli::cli_warn(c(
						"Group {.val {be}}/{.val {tiers[[ti]]$name}} has a chunk est. at {round(max(worst) / 3600, 1)}h, over its {round(walltime / 3600, 1)}h walltime.",
						i = "Lower {.arg target_seconds} or add a longer tier."
					))
				}
			}

			groups[[length(groups) + 1L]] <- list(
				backend = be,
				tier = tiers[[ti]]$name,
				resources = list(walltime = walltime, memory = grp_mem),
				jobs = grp[, .(job.id, chunk)]
			)
		}
	}

	# The whole point of the split: no chunk id may span two groups
	all_chunks <- unlist(lapply(groups, function(g) unique(g$jobs$chunk)))
	if (anyDuplicated(all_chunks)) {
		cli::cli_abort("Chunk ids collide across submission groups")
	}
	groups
}

# One line per submission group, for eyeballing before submitting.
report_groups <- function(groups) {
	if (length(groups) == 0L) {
		cli::cli_alert_info("No jobs to submit")
		return(invisible(groups))
	}
	for (g in groups) {
		n_chunks <- data.table::uniqueN(g$jobs$chunk)
		cli::cli_alert_info(
			"{g$backend}/{g$tier}: {nrow(g$jobs)} job{?s} in {n_chunks} chunk{?s}, walltime {round(g$resources$walltime / 3600)}h, mem {g$resources$memory}MB"
		)
	}
	invisible(groups)
}

# Submit every group as its own call, so backend and resource tier never mix
# within a session. Pass extra resources (e.g. ncpus) via `...`.
submit_groups <- function(groups, ...) {
	extra <- list(...)
	for (g in groups) {
		res <- utils::modifyList(g$resources, extra)
		batchtools::submitJobs(g$jobs, resources = res)
	}
	invisible(groups)
}

# One-shot resubmit of expired (OOM/walltime-killed) jobs with bumped memory,
# grouped by backend + resource tier like any other submission. For interactive
# use after an OOM wave:
#   resubmit_expired()                       # all expired, 2x memory, submit
#   resubmit_expired(factor = 4)             # 4x instead
#   resubmit_expired(submit = FALSE)         # build + print the plan, don't submit
#   resubmit_expired(runtimes = est$runtimes, base = est$memory)  # use estimates too
# `...` is forwarded to plan_submission() (e.g. target_seconds, chunk_size).
# Returns the plan invisibly.
resubmit_expired <- function(
	runtimes = NULL,
	base = NULL,
	factor = 2,
	submit = TRUE,
	...,
	reg = batchtools::getDefaultRegistry(),
	expired = batchtools::findExpired(reg = reg)
) {
	expired <- data.table::as.data.table(expired)
	if (nrow(expired) == 0L) {
		cli::cli_alert_success("No expired jobs to resubmit")
		return(invisible(NULL))
	}
	cli::cli_alert_info("Resubmitting {nrow(expired)} expired job{?s} at {factor}x memory")
	groups <- plan_submission(
		ids = expired,
		runtimes = runtimes,
		memory = escalate_memory(base = base, factor = factor, reg = reg, expired = expired),
		...
	)
	report_groups(groups)
	if (submit) {
		submit_groups(groups)
	}
	invisible(groups)
}
