# Runtime estimation from completed jobs, and reading estimates back.
#
# No top-level side effects (definitions only), so it is safe to source anywhere.
# Sourced by the eta.R lane scripts (to write) and run-experiment.R (to read).
#
# Only runtime is MODELLED here. Memory is not estimated from batchtools (that
# experiment didn't pan out); on the BIPS cluster memory comes from the external
# `slurm-memcheck` utility. write_memory_estimates() (below) turns its TSV into
# the mem-<prefix>.rds that read_estimates() picks up to size memory requests.

# Fit a batchtools runtime model on the finished jobs in a registry and write it
# to eta-<prefix>.rds at the project root.
#
# reg_path  registry path (conf$reg_path)
# prefix    lane tag, e.g. "importance" / "runtime"
# ids       optional job subset to train on. Either a data.table of job ids or a
#           function(reg) returning one -- use the function form for selectors
#           that need the loaded registry, e.g.
#           `\(reg) findExperiments(repls = 1:20, reg = reg)`. NULL uses all jobs.
# rf        ranger hyperparameters for estimateRuntimes()
# n_print   rows to print in the ETA table
write_estimates <- function(
	reg_path,
	prefix,
	ids = NULL,
	rf = list(num.trees = 1000, min.node.size = 10, mtry = 10, max.depth = 9),
	n_print = 1000
) {
	reg <- suppressMessages(batchtools::loadRegistry(reg_path, writeable = FALSE))
	if (is.function(ids)) {
		ids <- ids(reg)
	}
	tab <- batchtools::unwrap(batchtools::getJobPars(ids = ids, reg = reg))

	cli::cli_h1("Status")
	# Informational only; a scheduler hiccup must not lose the estimate
	try(batchtools::getStatus(tab, reg = reg), silent = TRUE)

	est <- do.call(batchtools::estimateRuntimes, c(list(tab), rf, list(reg = reg)))
	cli::cli_h1("ETA assuming {n_print} parallel jobs")
	print(est, n = n_print)
	cli::cli_inform("Runtime model R^2: {round(est$model$r.squared, 2)}")

	# Stamp which registry this came from. Estimates are keyed by job.id, which is
	# REGISTRY-LOCAL: an eta file from an earlier registry silently applies its
	# runtimes to whatever job now holds each id. That is not a small error -- it
	# fed bin-packing bogus per-job costs and produced a handful of chunks holding
	# hundreds of jobs each, where one OOM expired the lot.
	attr(est, "reg_stamp") <- list(
		reg_path = as.character(reg_path),
		n_jobs = nrow(batchtools::getJobTable(reg = reg))
	)

	out <- here::here(paste0("eta-", prefix, ".rds"))
	saveRDS(est, out)
	cli::cli_alert_success("Wrote {.file {out}}")
	invisible(est)
}

# Materialise `slurm-memcheck --tsv` output into the mem-<prefix>.rds that
# read_estimates() reads. This is the missing half of the memory story: eta.R
# writes runtimes, this writes measured memory.
#
# slurm-memcheck emits one row per Slurm job, which is one batchtools CHUNK: its
# `job_id` column is batchtools' `batch.id`, and `max_rss_mb` is the chunk's PEAK
# RSS across the jobs that ran in it. That peak is fanned out to every job.id in
# the chunk -- conservative for the light members, and exact once jobs run alone
# (resubmit_expired chunks alone by default, so a second slurm-memcheck pass after
# a clean run gives true per-job memory).
#
# Completed vs OOM rows are fundamentally different, and this is the one thing
# measurement CANNOT settle for you:
#   * COMPLETED -> max_rss is the true peak. No guessing; request it (+ the plan's
#     mem_headroom as scheduling margin).
#   * OUT_OF_MEMORY -> max_rss is pinned to the request it died under, so it is a
#     LOWER bound, not the need. How much more it actually wants is unknowable from
#     the log, so you must pick a multiplier. That is `oom_factor`, applied to the
#     failed request. It is the only real knob here -- measurement removed the guess
#     everywhere except the jobs that never got to finish.
#
# tsv        path to the TSV (or a data.frame of it). Columns required: job_id,
#            state, req_mem_mb, max_rss_mb.
# prefix     lane tag, matching read_estimates() / the eta files ("validation", ...).
# oom_factor multiplier on the FAILED request for OOM rows (default 2). An OOM
#            round is expensive (a whole resubmission wasted), over-provisioning is
#            cheap, so this errs high -- lower it if RAM is tight and you can afford
#            to climb over a few rounds. Does not touch completed jobs.
# reg        the registry the Slurm jobs belong to; defaults to the loaded one, so
#            after resume(lane) this just works.
# out        output path; defaults to mem-<prefix>.rds at the project root.
#
# Stamps the registry like write_estimates(), so read_estimates(reg_path=) refuses
# to apply it to a different registry (the estimates are keyed by job.id, which is
# registry-local).
write_memory_estimates <- function(
	tsv,
	prefix,
	oom_factor = 2,
	reg = batchtools::getDefaultRegistry(),
	out = NULL
) {
	mc <- if (is.data.frame(tsv)) {
		data.table::as.data.table(tsv)
	} else {
		data.table::fread(tsv)
	}
	need <- c("job_id", "state", "req_mem_mb", "max_rss_mb")
	miss <- setdiff(need, names(mc))
	if (length(miss) > 0) {
		cli::cli_abort("slurm-memcheck TSV is missing column{?s} {.val {miss}}.")
	}

	# Completed jobs: the measured peak. OOM jobs: the failed request scaled by
	# oom_factor, since the peak only tells us "at least this much, and it wasn't
	# enough". oom_factor is the guess measurement can't make for you.
	mc[,
		base_mb := data.table::fifelse(
			state == "OUT_OF_MEMORY",
			as.numeric(req_mem_mb) * oom_factor,
			as.numeric(max_rss_mb)
		)
	]

	jt <- data.table::as.data.table(batchtools::getJobTable(reg = reg))[, .(job.id, batch.id)]
	# batch.id is a list column -- one entry per (re)submission. The LAST is the run
	# slurm-memcheck just measured.
	jt[,
		batch.id := vapply(
			batch.id,
			function(b) if (length(b) > 0) as.integer(b[[length(b)]]) else NA_integer_,
			integer(1)
		)
	]
	jt <- jt[!is.na(batch.id)]

	m <- merge(
		jt,
		mc[, .(batch.id = as.integer(job_id), base_mb)],
		by = "batch.id"
	)
	if (nrow(m) == 0L) {
		cli::cli_abort(c(
			"No slurm-memcheck rows matched this registry's Slurm job ids.",
			"i" = "Widen {.code --since} to cover the runs, and check {.arg reg} is the right registry."
		))
	}

	memory <- m[, .(memory = ceiling(max(base_mb))), by = job.id]
	# Set the stamp last and save immediately -- data.table ops can drop attributes.
	attr(memory, "reg_stamp") <- list(
		reg_path = as.character(reg$file.dir),
		n_jobs = nrow(jt)
	)
	out <- out %||% here::here(paste0("mem-", prefix, ".rds"))
	saveRDS(memory, out)

	n_oom <- sum(mc$state == "OUT_OF_MEMORY", na.rm = TRUE)
	cli::cli_alert_success(
		"Wrote {.file {out}}: memory for {nrow(memory)} job{?s} from {nrow(mc)} Slurm job{?s} ({n_oom} OOM, scaled x{oom_factor})."
	)
	unmatched <- nrow(jt) - data.table::uniqueN(m$job.id)
	if (unmatched > 0) {
		cli::cli_alert_info(
			"{unmatched} registry job{?s} had no matching slurm-memcheck row (older than {.code --since}, or never run) -- {?it/they} fall{?s/} back to {.arg mem_default}."
		)
	}
	invisible(memory)
}

# Read estimates for a lane. Returns list(runtimes, memory), each a
# data.table(job.id, ...) or NULL when absent.
#
#   runtimes  from eta-<prefix>.rds  (written by write_estimates)
#   memory    from mem-<prefix>.rds  (external slurm-memcheck output, if you have it)
#
# Accepts both the full estimate object (uses $runtimes / $memory) and a bare
# data.table (legacy snapshots such as results/runtime-est.rds).
# reg_path  when given, estimate files stamped for a DIFFERENT registry are
#           discarded with a warning rather than silently misapplied (see the
#           stamp written by write_estimates()). Files with no stamp predate it
#           and are accepted, so old snapshots keep working.
read_estimates <- function(prefix, runtime_path = NULL, memory_path = NULL, reg_path = NULL) {
	check_stamp <- function(x, path) {
		stamp <- attr(x, "reg_stamp")
		if (is.null(reg_path)) {
			return(TRUE)
		}
		# No stamp means it cannot be verified, and an unverifiable estimate file is
		# exactly what caused the damage: one left over from an earlier registry was
		# applied by job.id to a new one. Discarding costs a chunking pass by job
		# count; accepting cost a submission.
		if (is.null(stamp)) {
			cli::cli_warn(c(
				"Ignoring {.file {fs::path_rel(path)}}: no registry stamp, so it cannot be matched to this registry.",
				"i" = "It is keyed by {.field job.id}, which is registry-local -- an older file's runtimes would be applied to whichever jobs now hold those ids.",
				"i" = "Re-run {.file {prefix}/eta.R} once this registry has finished jobs."
			))
			return(FALSE)
		}
		if (!identical(as.character(stamp$reg_path), as.character(reg_path))) {
			cli::cli_warn(c(
				"Ignoring {.file {fs::path_rel(path)}}: written for a different registry.",
				"i" = "It is keyed by {.field job.id}, which is registry-local, so its runtimes belong to other jobs.",
				"x" = "Stamped {.path {stamp$reg_path}}, current is {.path {reg_path}}.",
				"i" = "Re-run {.file {prefix}/eta.R} once this registry has finished jobs."
			))
			return(FALSE)
		}
		TRUE
	}
	pick <- function(path, field) {
		if (is.null(path) || !fs::file_exists(path)) {
			return(NULL)
		}
		x <- readRDS(path)
		if (!check_stamp(x, path)) {
			return(NULL)
		}
		# The estimate object (class RuntimeEstimate) also carries a data.table
		# class, so check its $runtimes/$memory element before the bare-dt case
		# (a legacy flat snapshot like results/runtime-est.rds).
		inner <- tryCatch(x[[field]], error = function(e) NULL)
		dt <- if (is.data.frame(inner)) {
			inner
		} else if (is.data.frame(x)) {
			x
		} else {
			NULL
		}
		if (is.null(dt)) NULL else data.table::as.data.table(dt)
	}
	list(
		runtimes = pick(runtime_path %||% here::here(paste0("eta-", prefix, ".rds")), "runtimes"),
		memory = pick(memory_path %||% here::here(paste0("mem-", prefix, ".rds")), "memory")
	)
}
