# Runtime estimation from completed jobs, and reading estimates back.
#
# No top-level side effects (definitions only), so it is safe to source anywhere.
# Sourced by the eta.R lane scripts (to write) and run-experiment.R (to read).
#
# Only runtime is modelled here. Memory is not estimated from batchtools (that
# experiment didn't pan out); on the BIPS cluster memory comes from the external
# `slurm-memcheck` utility. If you materialise its output as mem-<prefix>.rds,
# read_estimates() picks it up to size memory requests -- see read_estimates().

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
