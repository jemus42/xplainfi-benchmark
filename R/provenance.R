# Provenance and provider plumbing
#
# This file has no top-level side effects (only function definitions) so it is
# safe to `source()` in the main process or pass to batchtools `source =`.
#
# Two concerns live here:
#   1. Provider classification: which implementation an algorithm belongs to
#      ("xplainfi" = the package under test, "reference" = external comparison
#      implementations). Convention-based so new algorithms need no bookkeeping.
#   2. Provenance capture: pin a set of results to the exact xplainfi version and
#      git SHA they were produced with, so results can be retained and compared
#      across versions.

# ---------------------------------------------------------------------------
# Provider classification (convention over configuration)
# ---------------------------------------------------------------------------

# Reference implementations are named `<Method>_<pkg>` (e.g. PFI_iml,
# MarginalSAGE_fippy, MarginalSAGE_sage). xplainfi's own methods carry no
# provider suffix (PFI, CFI, LOCO, MarginalSAGE, ConditionalSAGE, ...).
# A future xplainfi kernel SAGE (e.g. `KernelSAGE`) is therefore classified as
# xplainfi automatically, while `KernelSAGE_sage` would be reference.
.reference_suffix <- "_(iml|vip|fippy|sage)$"

algo_provider <- function(algo) {
	data.table::fifelse(grepl(.reference_suffix, algo), "reference", "xplainfi")
}

# Reference implementations that run through reticulate / a Python backend.
algo_is_python <- function(algo) {
	grepl("_(fippy|sage)$", algo)
}

# Given a character vector of algorithm names and the requested providers,
# return the subset to register. `providers` may include "xplainfi",
# "reference", or "all".
select_algorithms <- function(algo_names, providers) {
	if ("all" %in% providers) {
		return(algo_names)
	}
	algo_names[algo_provider(algo_names) %in% providers]
}

# ---------------------------------------------------------------------------
# Provenance capture
# ---------------------------------------------------------------------------

# Read the resolved git SHA for a package from rv.lock. rv records the resolved
# commit even when the dependency tracks a moving branch, so this is the source
# of truth for the exact code that was benchmarked (the installed DESCRIPTION
# does not carry a RemoteSha under rv).
lockfile_sha <- function(pkg = "xplainfi", lockfile = here::here("rv.lock")) {
	if (!fs::file_exists(lockfile)) {
		return(NA_character_)
	}
	lines <- readLines(lockfile, warn = FALSE)
	idx <- grep(sprintf('^name = "%s"$', pkg), lines, fixed = FALSE)
	if (length(idx) == 0) {
		return(NA_character_)
	}
	# The `source = { ... sha = "..." }` line follows the name within a few lines.
	window <- lines[idx[1]:min(idx[1] + 8L, length(lines))]
	hit <- regmatches(window, regexpr('sha = "[0-9a-f]+"', window))
	hit <- hit[nzchar(hit)]
	if (length(hit) == 0) {
		return(NA_character_)
	}
	sub('sha = "([0-9a-f]+)"', "\\1", hit[1])
}

# One-row provenance record describing the environment a set of results was
# produced in. Deliberately small: xplainfi version + SHA are the primary keys;
# the run date is already stored per job in the batchtools registry.
capture_provenance <- function(providers = "all") {
	data.table::data.table(
		xplainfi_version = as.character(utils::packageVersion("xplainfi")),
		xplainfi_sha = lockfile_sha("xplainfi"),
		r_version = paste(R.version$major, R.version$minor, sep = "."),
		providers = paste(providers, collapse = ","),
		setup_time = format(Sys.time(), tz = "UTC", usetz = TRUE)
	)
}

# Persist / read the provenance record alongside a registry. Stored inside the
# registry directory so it travels with the registry; batchtools ignores unknown
# files in file.dir.
provenance_path <- function(reg_path) {
	fs::path(reg_path, "provenance.rds")
}

write_provenance <- function(reg_path, providers = "all") {
	prov <- capture_provenance(providers)
	saveRDS(prov, provenance_path(reg_path))
	prov
}

read_provenance <- function(reg_path) {
	f <- provenance_path(reg_path)
	if (fs::file_exists(f)) {
		readRDS(f)
	} else {
		NULL
	}
}

# ---------------------------------------------------------------------------
# Result reduction with provenance stamping
# ---------------------------------------------------------------------------

# Reduce a registry to a long importance table (one row per feature per job),
# joined with job parameters and stamped with provider + provenance columns.
# Returns an empty data.table if the registry has no results yet.
#
# The `provider`, `xplainfi_version`, and `xplainfi_sha` columns make each row
# self-describing, so tables from different xplainfi versions can be stacked and
# still be told apart. See combine_reduced() for the paired-comparison key.
reduce_importances <- function(reg, reg_path = reg$file.dir) {
	results <- batchtools::reduceResultsDataTable(reg = reg)
	if (nrow(results) == 0) {
		return(data.table::data.table())
	}

	# Unpack the nested per-job importance data.tables. Extraction mirrors the
	# proven pattern in collect-results.R: result[[1]]$importance is the inner
	# importance data.table for the (single) matched job.
	importances <- data.table::rbindlist(
		lapply(results$job.id, function(id) {
			imp <- results[job.id == id, result[[1]]$importance]
			if (is.null(imp)) {
				return(NULL)
			}
			imp[, job.id := id]
			imp
		}),
		fill = TRUE
	)

	# Per-job scalar result fields (broadcast across that job's feature rows on
	# merge). Anything an algo_* function *returns* rather than *receives* has to
	# be listed here or it never reaches the reduced table -- job parameters come
	# from getJobPars() below, but results are dropped unless named.
	#
	# n_evals is the coalition-evaluation count, the cost axis comparable across
	# estimators AND implementations; every SAGE arm reports it. budget_*/converged
	# come from SAGE's $budget and are what early stopping is judged on: `converged`
	# is FALSE when a ceiling was exhausted without meeting the criterion.
	scalar_fields <- c(
		runtime = "numeric",
		n_evals = "numeric",
		budget_requested = "numeric",
		budget_used = "numeric",
		converged = "logical"
	)
	scalars <- data.table::rbindlist(
		lapply(seq_len(nrow(results)), function(i) {
			r <- results$result[[i]]
			row <- data.table::data.table(job.id = results$job.id[i])
			for (f in names(scalar_fields)) {
				v <- r[[f]]
				row[,
					(f) := if (is.null(v) || length(v) != 1L) {
						as(NA, scalar_fields[[f]])
					} else {
						as(v, scalar_fields[[f]])
					}
				]
			}
			row
		}),
		fill = TRUE
	)

	pars <- batchtools::unwrap(batchtools::getJobPars(reg = reg))

	# getJobPars() selects only job.id/problem/prob.pars/algorithm/algo.pars --
	# `repl` lives on the job table. It is part of the documented paired-comparison
	# key, and without it a paired join silently becomes many-to-many across
	# replications, so pull it across here.
	repls <- data.table::as.data.table(batchtools::getJobTable(reg = reg))[,
		c("job.id", "repl"),
		with = FALSE
	]
	pars <- merge(pars, repls, by = "job.id", all.x = TRUE)

	out <- merge(importances, pars, by = "job.id", all.x = TRUE)
	out <- merge(out, scalars, by = "job.id", all.x = TRUE)

	prov <- read_provenance(reg_path)
	out[, provider := algo_provider(algorithm)]
	out[, xplainfi_version := if (is.null(prov)) NA_character_ else prov$xplainfi_version]
	out[, xplainfi_sha := if (is.null(prov)) NA_character_ else prov$xplainfi_sha]
	out[]
}

# ---------------------------------------------------------------------------
# Versioned reduced-table storage
# ---------------------------------------------------------------------------

# Reduced tables are the durable, version-stamped artifact; registries are
# treated as disposable scratch. One file per (lane, provider, version).
reduced_path <- function(lane, provider, version) {
	fs::path(
		here::here("results", lane),
		sprintf("%s-v%s.rds", provider, version)
	)
}

save_reduced <- function(dt, lane, provider = NULL, version = NULL) {
	if (is.null(provider)) {
		provider <- paste(sort(unique(as.character(dt$provider))), collapse = "+")
	}
	if (is.null(version)) {
		version <- as.character(dt$xplainfi_version[1])
	}
	path <- reduced_path(lane, provider, version)
	fs::dir_create(fs::path_dir(path))
	saveRDS(dt, path)
	cli::cli_alert_success("Saved reduced results to {.path {fs::path_rel(path)}}")
	path
}

load_reduced <- function(lane, provider, version) {
	path <- reduced_path(lane, provider, version)
	if (!fs::file_exists(path)) {
		cli::cli_abort("No reduced results at {.path {fs::path_rel(path)}}")
	}
	readRDS(path)
}

# Newest reduced table for a provider (by file mtime), or NULL if none exist.
# Use this to pull the "frozen" reference results to compare a fresh xplainfi run
# against, without needing to know their exact version string.
latest_reduced <- function(lane, provider) {
	dir <- here::here("results", lane)
	files <- list.files(
		dir,
		pattern = sprintf("^%s-v.*\\.rds$", provider),
		full.names = TRUE
	)
	if (length(files) == 0) {
		return(NULL)
	}
	info <- fs::file_info(files)
	readRDS(files[which.max(info$modification_time)])
}

# Stack a freshly-collected xplainfi table with a frozen reference table (or any
# set of reduced tables) into one long table for analysis. NULLs are dropped.
#
# Rows align because instances are synchronised across registries by the
# batchtools problem seed (problem.seed + repl - 1), independent of job.id. For
# *paired* comparisons (e.g. bias of a method vs its reference on the same
# instance) join on the semantic key:
#   c("problem", "learner_type", "sampler", "feature", "repl", <problem params>)
# NOT on job.id, which is registry-local.
combine_reduced <- function(...) {
	tables <- Filter(function(x) !is.null(x) && nrow(x) > 0, list(...))
	if (length(tables) == 0) {
		return(data.table::data.table())
	}
	data.table::rbindlist(tables, fill = TRUE, use.names = TRUE)
}
