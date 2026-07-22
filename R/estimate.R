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
read_estimates <- function(prefix, runtime_path = NULL, memory_path = NULL) {
	pick <- function(path, field) {
		if (is.null(path) || !fs::file_exists(path)) {
			return(NULL)
		}
		x <- readRDS(path)
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

# Per-job memory estimates from the registry's own measurements -> mem-<prefix>.rds,
# which read_estimates() already picks up and plan_submission() sizes requests from.
#
# `measure.memory = TRUE` in batchtools.conf.R makes batchtools record `mem.used`
# (MB) per finished job, so the memory side needs no external tool: run the pilot,
# call this, and the next submission is sized from observation rather than the flat
# `mem_default`.
#
# Estimation is deliberately blunt -- the peak observed within a design cell, not a
# model. Memory here is driven by the cell (algorithm x estimator x budget x data
# size), not by anything a regression would find that `max` would not, and an
# under-estimate costs a dead chunk while an over-estimate costs queue time.
#
# CAVEAT: `mem.used` comes from gc() inside the R session, so it measures the R
# heap. It undercounts anything allocated outside it -- notably reticulate/Python
# jobs, where the interpreter's memory is invisible. Hence `python_headroom`,
# applied on top of `headroom` for jobs carrying the `python` tag.
#
# reg_path         registry path (conf$reg_path)
# prefix           lane tag, "importance" / "runtime"
# headroom         multiplier on the observed peak
# python_headroom  extra multiplier for python-tagged jobs (see caveat)
# min_memory       floor in MB, so trivially small jobs still get a sane request
write_memory_estimates <- function(
	reg_path,
	prefix,
	headroom = 1.5,
	python_headroom = 2,
	min_memory = 1024
) {
	reg <- suppressMessages(batchtools::loadRegistry(reg_path, writeable = FALSE))
	tab <- batchtools::unwrap(batchtools::getJobTable(reg = reg))

	if (!("mem.used" %in% names(tab))) {
		cli::cli_abort(c(
			"Registry has no {.field mem.used} column.",
			"i" = "Set {.code measure.memory = TRUE} in {.file batchtools.conf.R} and re-run some jobs."
		))
	}
	done <- tab[!is.na(mem.used)]
	if (nrow(done) == 0L) {
		cli::cli_abort(c(
			"No finished jobs carry a {.field mem.used} measurement yet.",
			"i" = "Run the pilot pass first: {.code XPLAINFI_BENCH_REPLS=1 Rscript {prefix}/run-experiment.R}"
		))
	}

	# The design cell: everything that plausibly moves memory, minus the
	# replication index. Whatever columns this lane happens to carry.
	cell_key <- intersect(
		c(
			"problem",
			"algorithm",
			"learner_type",
			"sampler",
			"estimator",
			"kernel_variant",
			"n_samples",
			"n_features",
			"sage_n_samples",
			"n_coalitions",
			"n_permutations",
			"n_repeats",
			"correlation"
		),
		names(tab)
	)

	peaks <- done[, .(cell_peak = max(mem.used)), by = cell_key]
	out <- merge(tab[, c("job.id", cell_key), with = FALSE], peaks, by = cell_key, all.x = TRUE)

	# Cells never observed (the pilot covers each once, but a failed job leaves a
	# hole) fall back to the global peak rather than the default -- a hole is more
	# likely to be an expensive cell than a cheap one.
	global_peak <- max(done$mem.used)
	out[is.na(cell_peak), cell_peak := global_peak]

	py <- data.table::as.data.table(batchtools::findTagged("python", reg = reg))
	out[, memory := pmax(ceiling(cell_peak * headroom), min_memory)]
	if (nrow(py) > 0) {
		out[
			job.id %in% py$job.id,
			memory := pmax(ceiling(cell_peak * headroom * python_headroom), min_memory)
		]
	}

	est <- list(memory = out[, .(job.id, memory)])
	path <- here::here(paste0("mem-", prefix, ".rds"))
	saveRDS(est, path)

	cli::cli_alert_success(
		"Wrote {.file {path}}: {nrow(out)} job{?s} from {nrow(done)} measurement{?s} across {nrow(peaks)} cell{?s}"
	)
	cli::cli_alert_info(
		"Memory request range: {min(out$memory)}-{max(out$memory)} MB (observed peak {round(global_peak)} MB)"
	)
	invisible(est)
}
