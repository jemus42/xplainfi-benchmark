library(batchtools)
library(data.table)
library(dplyr)
library(ggplot2)

source("config.R")
reg <- loadRegistry(
	"registries/importance/xplainfi-xplainfi-0.2.1/",
	writeable = FALSE,
	work.dir = here::here()
)
tab <- unwrap(getJobTable())
results <- reduceResultsDataTable()

tmpres = data.table::rbindlist(results$result, fill = TRUE)
tmpres = cbind(results[, .(job.id)], tmpres)

res = ijoin(
	tmpres,
	unwrap(getJobPars())[, .(
		job.id,
		problem,
		algorithm,
		learner_type,
		# n_samples,
		# n_features,
		correlation,
		n_repeats,
		sampler,
		n_permutations,
		sage_n_samples
	)],
	by = "job.id"
)


# Extract importances
importances = rbindlist(
	lapply(results$job.id, \(x) {
		importances = results[job.id == x, result[[1]]$importance]
		importances[, job.id := x]
	}),
	fill = TRUE
)

importances = merge(res[, -"importance"], importances, by = "job.id")

importances |>
	dplyr::filter(problem == "correlated") |>
	ggplot(aes(x = importance, y = feature, fill = algorithm)) +
	geom_boxplot()


getStatus()


importances |>
	filter(problem == "bike_sharing") |>
	ggplot(aes(y = feature, x = importance, fill = algorithm)) +
	facet_wrap(vars(feature), scales = "free") +
	geom_boxplot()

importances |>
	# filter(!is.na(n_permutations)) |>
	filter(grepl("SAGE", algorithm)) |>
	select(algorithm, sampler, n_permutations, n_permutations_used) |>
	unique()
