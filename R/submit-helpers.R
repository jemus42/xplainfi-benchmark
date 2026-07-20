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
# Intersect with a scope for a pilot pass, e.g.
#   ids <- ijoin(findExperiments(repls = 1), todo())
todo <- function(reg = batchtools::getDefaultRegistry()) {
	batchtools::findNotDone(reg = reg) |>
		batchtools::ajoin(batchtools::findRunning(reg = reg)) |>
		batchtools::ajoin(batchtools::findQueued(reg = reg))
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
	ids[, backend := data.table::fifelse(
		job.id %in% data.table::as.data.table(python)$job.id,
		"python",
		"r"
	)]
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
