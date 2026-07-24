# Job grouping for submission
#
# Sourced by the `run-experiment.R` scripts only -- not shipped to workers.
#
# Jobs are grouped on two axes, in order:
#
#   1. Partition (plan_submission's `group_by`, default `backend_split`: R vs
#      Python). batchtools runs every job sharing a `chunk` value sequentially in
#      ONE R session, so R-backed jobs (mlr3torch -> libtorch) and Python-backed
#      jobs (reticulate -> torch) must never share a chunk, or two torch runtimes
#      load into the same process. This is the only benchmark-specific axis; the
#      rest generalises to any batchtools project.
#
#   2. Resource tier. A single submitJobs() call carries ONE `resources` list,
#      so the jobs in it must have aligned requirements. Tiers bucket by
#      estimated runtime; each tier maps to a requested walltime. QoS is
#      derived from walltime by the Slurm template, so it is NOT set here.
#      Memory is taken from pretested estimates when available.
#
# The output is a flat list of submission groups; each is one submitJobs() call.

# Start-of-session helper for an ongoing benchmark lane. Sources the lane config
# for its registry path, loads the registry WRITEABLE (so you can resubmit),
# reads any runtime/memory estimates, prints status, then prints the runbook
# (bench_help) so the next commands are in front of you. Replaces the
# source/loadRegistry/getStatus/read_estimates boilerplate with one call, and
# returns list(reg, conf, est, lane).
#
# loadRegistry sets the default registry, so todo()/resubmit_expired() find it
# without a `reg =` argument. `version` overrides XPLAINFI_BENCH_VERSION to target
# a specific registry (e.g. an ongoing pretest); NULL uses the lane's default.
resume <- function(lane, version = NULL) {
	if (!is.null(version)) {
		old <- Sys.getenv("XPLAINFI_BENCH_VERSION", unset = NA_character_)
		Sys.setenv(XPLAINFI_BENCH_VERSION = version)
		on.exit(
			if (is.na(old)) {
				Sys.unsetenv("XPLAINFI_BENCH_VERSION")
			} else {
				Sys.setenv(XPLAINFI_BENCH_VERSION = old)
			}
		)
	}
	# Source the lane config into its own env so `conf` does not leak into globals.
	e <- new.env()
	sys.source(here::here(lane, "config.R"), envir = e)
	conf <- e$conf
	reg <- batchtools::loadRegistry(conf$reg_path, writeable = TRUE, work.dir = here::here())
	est <- read_estimates(lane, reg_path = conf$reg_path)
	print(batchtools::getStatus(reg = reg))
	bench_help(lane)
	invisible(list(reg = reg, conf = conf, est = est, lane = lane))
}

# The runbook, printed with the current signatures and the lane filled in. Called
# by resume() at session start, and callable any time you forget the incantation.
# It lives next to the functions on purpose: change a signature and you fix the
# recipe in the same diff, so it can't drift the way a wiki (or your memory) does.
bench_help <- function(lane = "<lane>") {
	q <- function(x) sprintf('"%s"', x) # quoted lane, or the literal <lane> token
	ql <- if (identical(lane, "<lane>")) lane else q(lane)
	cli::cli_h2("Benchmark workflow {.emph ({lane})}")
	cli::cli_text("Start / check status:")
	cli::cli_code(sprintf("b <- resume(%s)", ql))
	cli::cli_text(
		"Resubmit expired, memory sized from measurement (no {.path mem.tsv} file needed --"
	)
	cli::cli_text("{.fn fread} runs slurm-memcheck and reads its output directly):")
	cli::cli_code(c(
		sprintf(
			'mc  <- data.table::fread(cmd = "slurm-memcheck --since now-1day --tsv")'
		),
		sprintf("write_memory_estimates(mc, %s)", ql),
		sprintf("est <- read_estimates(%s, reg_path = b$conf$reg_path)", ql),
		"g   <- resubmit_expired(runtimes = est$runtimes, base = est$memory, submit = FALSE)",
		"report_groups(g); submit_groups(g)"
	))
	cli::cli_text("Add a deadline (hours until it must be done):")
	cli::cli_code(
		"resubmit_expired(runtimes = est$runtimes, base = est$memory, max_walltime_h = 6)"
	)
	cli::cli_text("Reprint this: {.code bench_help()}")
	invisible()
}

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
# Abort with an actionable message when the registry a script is about to load
# does not exist. batchtools' own error names the missing directory but not the
# reason, and the two reasons need opposite fixes:
#   * nothing built yet          -> run setup-batchtools.R
#   * XPLAINFI_BENCH_VERSION unset/mismatched -> the registry exists under
#     another version segment, and rebuilding would be wrong
# Listing the siblings makes which one it is obvious at a glance.
require_registry <- function(reg_path, lane = basename(dirname(reg_path))) {
	if (fs::dir_exists(reg_path)) {
		return(invisible(reg_path))
	}
	siblings <- fs::path_file(fs::dir_ls(fs::path_dir(reg_path), type = "directory"))
	msg <- c(
		"No registry at {.path {fs::path_rel(reg_path)}}.",
		"i" = "Build it first: {.code Rscript {lane}/setup-batchtools.R}"
	)
	if (length(siblings) > 0) {
		msg <- c(
			msg,
			"!" = "Other registries exist for this lane: {.val {siblings}}.",
			"i" = "If one of those is the one you want, set {.envvar XPLAINFI_BENCH_VERSION} to its version segment rather than rebuilding."
		)
	}
	cli::cli_abort(msg)
}

# todo(), optionally narrowed to the replications named by XPLAINFI_BENCH_REPLS
# ("1", or "1,2"). Unset means everything outstanding.
#
# This exists so the pilot pass is an env var rather than an edit: replication 1
# covers every design cell exactly once, which is the coverage
# batchtools::estimateRuntimes() needs before eta.R can model anything, and a
# fresh registry has no completed jobs to learn from.
todo_repls <- function(
	repls = Sys.getenv("XPLAINFI_BENCH_REPLS", unset = ""),
	reg = batchtools::getDefaultRegistry()
) {
	if (!nzchar(repls)) {
		return(todo(reg = reg))
	}
	# anyNA() below is the real guard; the coercion warning would only be noise.
	repls <- suppressWarnings(as.integer(trimws(strsplit(repls, ",")[[1]])))
	if (anyNA(repls)) {
		cli::cli_abort(
			"{.envvar XPLAINFI_BENCH_REPLS} must be comma-separated integers, got {.val {repls}}."
		)
	}
	ids <- todo(repls = repls, reg = reg)
	cli::cli_alert_info(
		"{.envvar XPLAINFI_BENCH_REPLS}={.val {repls}}: {nrow(ids)} outstanding job{?s}"
	)
	ids
}

# Why an expired job died, from its Slurm log. batchtools' findExpired() cannot
# tell the two apart -- both are "started, never wrote a result" -- but the two
# need opposite fixes, and applying the wrong one loops forever: a walltime-kill
# resubmitted with double memory and the same walltime dies identically.
#
# Split out as a pure function on the log text so it is testable without a
# cluster (see tests/test-submit-helpers.R). Markers are arguments because they
# are Slurm/template wording, not batchtools API.
classify_expiry_log <- function(
	log_text,
	timeout_markers = c("DUE TO TIME LIMIT", "TIME LIMIT"),
	oom_markers = c("oom-kill", "Out Of Memory", "Exceeded job memory limit", "OUT_OF_MEMORY")
) {
	if (length(log_text) == 0L || all(is.na(log_text))) {
		return("unknown")
	}
	txt <- paste(log_text, collapse = "\n")
	hit <- function(m) any(vapply(m, grepl, logical(1), x = txt, fixed = TRUE))
	# Timeout first: an OOM-killed job can also be reported as cancelled, but a
	# time-limit message is unambiguous.
	if (hit(timeout_markers)) {
		return("timeout")
	}
	if (hit(oom_markers)) {
		return("oom")
	}
	"unknown"
}

# Classify every expired job. Unreadable or unrecognised logs are "unknown",
# which callers escalate on BOTH axes -- over-provisioning is recoverable, an
# infinite resubmit loop is not.
expired_reasons <- function(
	reg = batchtools::getDefaultRegistry(),
	expired = batchtools::findExpired(reg = reg)
) {
	expired <- data.table::as.data.table(expired)
	if (nrow(expired) == 0L) {
		return(data.table::data.table(job.id = integer(), reason = character()))
	}
	reasons <- vapply(
		expired$job.id,
		function(id) {
			txt <- tryCatch(
				batchtools::getLog(id, reg = reg),
				error = function(e) NA_character_
			)
			classify_expiry_log(txt)
		},
		character(1)
	)
	data.table::data.table(job.id = expired$job.id, reason = reasons)
}

escalate_memory <- function(
	base = NULL,
	factor = 2,
	reg = batchtools::getDefaultRegistry(),
	expired = batchtools::findExpired(reg = reg),
	reasons = NULL
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
	# Only bump memory for jobs that plausibly ran out of it. Doubling memory on a
	# walltime-kill wastes an allocation and, because the tier is unchanged, the
	# resubmit dies exactly the same way.
	if (!is.null(reasons)) {
		keep <- data.table::as.data.table(reasons)[reason %in% c("oom", "unknown"), job.id]
		expired <- expired[job.id %in% keep]
		if (nrow(expired) == 0L) {
			return(out)
		}
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

# The walltime counterpart of escalate_memory(): bump the *runtime estimate* of
# jobs that were killed by the time limit, so plan_submission()'s existing
# tier_of() lookup promotes them to a longer-walltime tier.
#
# Inflating the estimate rather than overriding the walltime directly is
# deliberate -- `runtimes` already drives both tiering and bin-packing, so one
# change gets the job a longer walltime AND stops it sharing a chunk with short
# jobs. There is no per-job walltime to override: plan_submission() issues one
# `resources` list per group.
#
# The basis is the walltime the job was *given*, not how long it ran (batchtools
# records no completion time for a job that never finished), so `factor` is
# relative to the previous request.
escalate_runtime <- function(
	base = NULL,
	factor = 2,
	reg = batchtools::getDefaultRegistry(),
	expired = batchtools::findExpired(reg = reg),
	reasons = NULL,
	tiers = default_tiers
) {
	out <- if (is.null(base)) {
		data.table::data.table(job.id = integer(), runtime = numeric())
	} else {
		data.table::as.data.table(base)[, .(job.id, runtime)]
	}
	expired <- data.table::as.data.table(expired)
	if (nrow(expired) == 0L) {
		return(out)
	}
	if (!is.null(reasons)) {
		keep <- data.table::as.data.table(reasons)[reason %in% c("timeout", "unknown"), job.id]
		expired <- expired[job.id %in% keep]
		if (nrow(expired) == 0L) {
			return(out)
		}
	}

	res <- batchtools::getJobResources(expired, reg = reg)
	req <- res[, .(
		job.id,
		runtime = factor * vapply(resources, function(r) as.numeric(r$walltime %||% NA), numeric(1))
	)]

	# Nothing above the last tier, so a job killed there cannot be given more.
	# Silently resubmitting it would loop forever.
	ceiling_rt <- tiers[[length(tiers)]]$walltime
	stuck <- req[!is.na(runtime) & runtime > factor * ceiling_rt]
	if (nrow(stuck) > 0) {
		cli::cli_warn(c(
			"{nrow(stuck)} job{?s} already expired at the longest tier ({round(ceiling_rt / 3600)}h).",
			"i" = "Resubmitting cannot give {?it/them} more walltime -- split the work or add a tier to {.fun default_tiers}."
		))
	}

	data.table::rbindlist(list(out[!job.id %in% req$job.id], req), use.names = TRUE)
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
# group_by       function(ids) -> character label per job. Jobs with different
#                labels never share a chunk. This is the one benchmark-specific
#                seam: the default `backend_split` keeps R (mlr3torch/libtorch) and
#                Python (reticulate torch) jobs apart, since they collide in one R
#                session. A project with no such constraint can pass
#                `\(ids) rep("all", nrow(ids))` to disable partitioning, or any
#                other partitioning rule. Everything else here is generic.
# runtimes       optional data.table(job.id, runtime[seconds]) from eta.R. Drives
#                both tiering and bin-packing. Jobs with no estimate go to the
#                last (safest) tier and chunk alone.
# memory         optional data.table(job.id, memory[MB]) from a pretest run.
# tiers          see default_tiers
# target_seconds bin-pack chunks up to this wall-clock (< the tier walltime, to
#                leave headroom)
# max_walltime_h deadline cap (hours). Requests no more than this walltime, packs
#                chunks to fit under it, and EXCLUDES jobs whose own estimate
#                already exceeds it (they cannot finish in the window). NULL = off.
#                Use it when jobs must be done by a wall-clock time (e.g. before
#                the cluster gets noisy at 07:00): pass the hours remaining.
# mem_headroom   multiply the per-group max estimate by this
# mem_default    requested memory (MB) when no estimate is available
# chunk_size     jobs per chunk when no runtimes are given
#
# Returns a list of groups, each: list(group, tier, resources, jobs), where
# `jobs` is data.table(job.id, chunk) ready for submitJobs(). Groups with no
# jobs are omitted.
#
# The default partition for this benchmark: R vs Python backend. A chunk runs in
# one R session, so mlr3torch's libtorch and reticulate's Python torch must never
# share one. Passed to plan_submission() as `group_by` -- the only benchmark-
# specific piece; swap it for any partitioning rule (or none) in another project.
backend_split <- function(ids, reg = batchtools::getDefaultRegistry()) {
	py <- data.table::as.data.table(batchtools::findTagged("python", reg = reg))$job.id
	data.table::fifelse(data.table::as.data.table(ids)$job.id %in% py, "python", "r")
}

plan_submission <- function(
	ids,
	group_by = backend_split,
	runtimes = NULL,
	memory = NULL,
	tiers = default_tiers,
	target_seconds = 12 * 3600,
	max_walltime_h = NULL,
	mem_headroom = 1.3,
	mem_default = 4 * 1024,
	chunk_size = 20L,
	max_chunk_jobs = 25L,
	pilot = FALSE
) {
	# Pilot: nothing has run yet, so any runtime estimate is either absent or --
	# worse -- left over from a different registry, where it is keyed by a job.id
	# that now means something else. Bin-packing on those produced 3-4 chunks of
	# several hundred jobs each, and because a chunk is ONE Slurm job, the first
	# OOM took all of them down. Chunk by count, small, and request the longest
	# walltime and generous memory: a pilot is measuring cost, not economising.
	if (pilot) {
		runtimes <- NULL
		memory <- NULL
		chunk_size <- min(chunk_size, 4L)
		max_chunk_jobs <- min(max_chunk_jobs, 4L)
		tiers <- tiers[length(tiers)]
		mem_default <- max(mem_default, 8 * 1024)
		cli::cli_alert_info(
			"Pilot mode: {.val {chunk_size}} job{?s}/chunk, {round(tiers[[1]]$walltime / 3600)}h walltime, {mem_default}MB, estimates ignored."
		)
	}
	# Deadline mode: a hard cap on requested walltime (e.g. the hours left before
	# the cluster gets noisy at 07:00). Cap the pack target so a full chunk still
	# fits under it; the walltime request and the too-long-job exclusion happen
	# below once runtimes are known.
	max_wt_s <- if (!is.null(max_walltime_h)) max_walltime_h * 3600 else Inf
	if (is.finite(max_wt_s) && is.null(runtimes)) {
		cli::cli_warn(c(
			"{.arg max_walltime_h} caps the requested walltime but without {.arg runtimes} it cannot size chunks to fit or exclude too-long jobs.",
			"i" = "Chunks may overrun the deadline. Run eta.R and pass its estimates first."
		))
	}
	if (is.finite(max_wt_s)) {
		# Pack to 80% of the cap but request the full cap, so a chunk has ~20% slack
		# for optimistic estimates before it risks the deadline. A single job longer
		# than the target still chunks alone and gets the full cap; only jobs longer
		# than the cap itself are excluded (below). Set the cap below the true
		# deadline as well for a hard cut-off.
		target_seconds <- min(target_seconds, max_wt_s * 0.8)
	}

	ids <- data.table::as.data.table(ids)[, .(job.id)]
	ids[, group := as.character(group_by(ids))]
	if (anyNA(ids$group)) {
		cli::cli_abort("{.arg group_by} returned NA for {sum(is.na(ids$group))} job{?s}.")
	}
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

	# Deadline exclusion: a job whose own estimate exceeds the cap cannot finish in
	# the window, so drop it rather than request the cap and have Slurm kill it at
	# the deadline. NA-estimate jobs are kept (they chunk alone) but flagged, since
	# without an estimate we cannot promise they fit.
	if (is.finite(max_wt_s)) {
		over <- ids[!is.na(runtime) & runtime > max_wt_s]
		if (nrow(over) > 0L) {
			cli::cli_warn(c(
				"{nrow(over)} job{?s} estimated over the {round(max_wt_s / 3600, 1)}h cap -- excluded (cannot finish by the deadline).",
				"i" = "Submit the excluded jobs without {.arg max_walltime_h} once the deadline has passed."
			))
			ids <- ids[is.na(runtime) | runtime <= max_wt_s]
		}
		n_na <- sum(is.na(ids$runtime))
		if (n_na > 0L) {
			cli::cli_alert_warning(
				"{n_na} job{?s} without a runtime estimate kept under the cap -- each chunks alone and requests the full cap, but could overrun it."
			)
		}
	}

	groups <- list()
	offset <- 0L
	for (gv in sort(unique(ids$group))) {
		for (ti in seq_along(tiers)) {
			grp <- ids[group == gv & tier_i == ti]
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
			# Hard cap on jobs per chunk, whichever path built them. A chunk is one
			# Slurm job running its members sequentially, so its size is the blast
			# radius of a single OOM or overrun: every job in it lands in
			# findExpired(), most of them innocent. Bin-packing to a wall-clock
			# target alone ignores this -- many short jobs pack into one enormous
			# chunk.
			grp[,
				chunk := {
					sub <- seq_len(.N) - 1L
					chunk * 1000L + (sub %/% max_chunk_jobs)
				},
				by = chunk
			]
			grp[, chunk := as.integer(factor(chunk))]
			grp[, chunk := chunk + offset * 1e6L]

			est <- grp$memory[!is.na(grp$memory)]
			grp_mem <- if (length(est) > 0) {
				max(ceiling(max(est) * mem_headroom), mem_default)
			} else {
				mem_default
			}

			# min() with Inf (no cap) leaves the tier walltime untouched.
			walltime <- min(tiers[[ti]]$walltime, max_wt_s)
			if (!is.null(runtimes)) {
				worst <- grp[, sum(rt), by = chunk][["V1"]]
				if (any(worst > walltime)) {
					cli::cli_warn(c(
						"Group {.val {gv}}/{.val {tiers[[ti]]$name}} has a chunk est. at {round(max(worst) / 3600, 1)}h, over its {round(walltime / 3600, 1)}h walltime.",
						i = "Lower {.arg target_seconds} or add a longer tier."
					))
				}
			}

			groups[[length(groups) + 1L]] <- list(
				group = gv,
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
			"{g$group}/{g$tier}: {nrow(g$jobs)} job{?s} in {n_chunks} chunk{?s}, walltime {round(g$resources$walltime / 3600)}h, mem {g$resources$memory}MB"
		)
	}
	invisible(groups)
}

# Submit every group as its own call, so partition and resource tier never mix
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
	# OOM and walltime-kill need opposite fixes, and applying the wrong one loops:
	# a timed-out job resubmitted with double memory and the same tier dies the
	# same way. Classify from the Slurm log; "unknown" gets both, since
	# over-provisioning is recoverable and an infinite resubmit loop is not.
	reasons <- expired_reasons(reg = reg, expired = expired)
	tally <- reasons[, .N, by = reason][order(-N)]
	cli::cli_alert_info(
		"Resubmitting {nrow(expired)} expired job{?s} at {factor}x: {paste(tally$reason, tally$N, sep = '=', collapse = ', ')}"
	)
	if ("unknown" %in% reasons$reason) {
		cli::cli_alert_warning(
			"{sum(reasons$reason == 'unknown')} job{?s} had no recognisable reason in the log; escalating both memory and walltime."
		)
	}

	# Chunk expired jobs ALONE by default. batchtools runs a chunk's jobs
	# sequentially in one Slurm job, so a single OOM or overrun tears down the
	# whole chunk and every job in it lands in findExpired() -- most of them
	# innocent. Re-packing that same set together reproduces the coupling and can
	# re-kill the bystanders indefinitely; one job per chunk isolates the culprit,
	# and the next round's estimates are then per-job rather than per-chunk.
	dots <- list(...)
	if (is.null(dots$chunk_size)) {
		dots$chunk_size <- 1L
	}

	groups <- do.call(
		plan_submission,
		c(
			list(
				ids = expired,
				runtimes = escalate_runtime(
					base = runtimes,
					factor = factor,
					reg = reg,
					expired = expired,
					reasons = reasons
				),
				memory = escalate_memory(
					base = base,
					factor = factor,
					reg = reg,
					expired = expired,
					reasons = reasons
				)
			),
			dots
		)
	)
	report_groups(groups)
	if (submit) {
		submit_groups(groups)
	}
	invisible(groups)
}

# What actually happened to the expired jobs, chunk by chunk.
#
# The unit of failure is the CHUNK, not the job: batchtools runs a chunk's jobs
# sequentially in one Slurm job sharing one log file, so a job that is killed
# takes every not-yet-run job in its chunk down with it. Those bystanders are
# indistinguishable from the culprit in findExpired() -- and a chunk log that
# ends with "Job terminated successfully" is the signature, since it means the
# chunk died in the NEXT job, which never got far enough to log anything.
#
# Reports one row per chunk containing expired jobs:
#   n_expired / n_done   how far the chunk got before dying
#   last_ok              the last job.id that completed (the log's final line)
#   batch.id             the Slurm job id, to hand to sacct for the real cause:
#                          sacct -j <batch.id> -o JobID,State,MaxRSS,ReqMem,Elapsed
#   reason               classify_expiry_log() on the shared log
#
# batchtools cannot tell OOM from walltime-kill itself, and its own `mem.used` is
# a gc()-based R-heap figure that misses allocations outside R. sacct (or the
# slurm-memcheck utility that parses it) is the authority on both.
expired_overview <- function(
	reg = batchtools::getDefaultRegistry(),
	expired = batchtools::findExpired(reg = reg),
	log_lines = 3L
) {
	# Return the full schema even when empty, so a caller's column selection does
	# not error on the happy path.
	empty <- data.table::data.table(
		batch.id = character(),
		n_jobs = integer(),
		n_done = integer(),
		n_expired = integer(),
		last_ok = character(),
		reason = character(),
		log_tail = character()
	)
	expired <- data.table::as.data.table(expired)
	if (nrow(expired) == 0L) {
		cli::cli_alert_success("No expired jobs")
		return(invisible(empty))
	}

	tab <- data.table::as.data.table(batchtools::getJobTable(reg = reg))
	done <- data.table::as.data.table(batchtools::findDone(reg = reg))$job.id

	# Chunk membership is not in the job table; recover it from the shared log
	# file, which is one per Slurm job and therefore one per chunk.
	tab[, .chunk := log.file]
	chunks <- tab[job.id %in% expired$job.id, unique(.chunk)]

	out <- data.table::rbindlist(
		lapply(chunks, function(lf) {
			members <- tab[.chunk == lf]
			txt <- tryCatch(
				batchtools::getLog(members$job.id[1], reg = reg),
				error = function(e) NA_character_
			)
			data.table::data.table(
				batch.id = members$batch.id[1],
				n_jobs = nrow(members),
				n_done = sum(members$job.id %in% done),
				n_expired = sum(members$job.id %in% expired$job.id),
				last_ok = {
					hits <- grep("terminated successfully", txt, value = TRUE)
					if (length(hits) == 0) NA_character_ else utils::tail(hits, 1)
				},
				reason = classify_expiry_log(txt),
				log_tail = paste(utils::tail(txt[!is.na(txt)], log_lines), collapse = " | ")
			)
		}),
		fill = TRUE
	)

	setorder(out, -n_expired)
	cli::cli_alert_info(
		"{nrow(expired)} expired job{?s} across {nrow(out)} chunk{?s}; reasons: {paste(out[, .N, by = reason][, paste0(reason, '=', N)], collapse = ', ')}"
	)
	if (any(out$n_done > 0)) {
		cli::cli_alert_warning(
			"{sum(out$n_done > 0)} chunk{?s} had jobs succeed before dying -- those chunk-mates expired as bystanders, not on their own merits."
		)
	}
	cli::cli_alert_info(
		"Real cause is in sacct: {.code sacct -j <batch.id> -o JobID,State,MaxRSS,ReqMem,Elapsed}"
	)
	out[]
}
