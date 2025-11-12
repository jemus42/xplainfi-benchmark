library(batchtools)
library(data.table)
library(dplyr)
library(ggplot2)

source("config.R")
reg <- loadRegistry(
	"registries/runtime/xplainfi-0.2.0/",
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
		n_repeats,
		sampler,
		n_permutations,
		sage_n_samples
	)],
	by = "job.id"
)
table(res$algorithm)
table(res$problem)
table(res$n_samples)
table(res$n_features)
table(res$sage_n_samples)
table(res$n_permutations)


res[
	algorithm == "MarginalSAGE_sage" & sage_n_samples == 200 & n_features == 5 & n_samples == 500,
	importance
]

res[
	algorithm == "PFI" &
		n_repeats == 100 &
		n_features == 5 &
		n_samples == 500 &
		learner_type == "linear",
	importance
]


res |>
	filter(n_features == 5, n_samples == 100) |>
	ggplot(aes(x = runtime, y = paste(algorithm, sampler, sep = " "))) +
	facet_wrap(vars(n_samples, n_features)) +
	geom_boxplot()

res |>
	filter(algorithm == "MarginalSAGE_sage") |>
	View()


res |>
	filter(stringr::str_detect(algorithm, "SAGE")) |>
	mutate(algorithm = glue::glue("{algorithm} {sampler}", .na = "")) |>
	filter(n_features == 5, n_samples == 100) |>
	ggplot(aes(x = runtime, y = reorder(algorithm, runtime))) +
	facet_wrap(vars(n_samples, n_features)) +
	geom_boxplot()
