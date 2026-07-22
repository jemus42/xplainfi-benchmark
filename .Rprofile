#source("renv/activate.R")
source("rv/scripts/rvr.R")
source("rv/scripts/activate.R")
# Ensure ranger behaves, particularly important for nested parallelization here with conditional sampling depending on ranger as well
options(ranger.num.threads = 2)
Sys.setenv(OMP_NUM_THREADS = 2)
Sys.setenv(OMP_THREAD_LIMIT = 2)
Sys.setenv(MKL_NUM_THREADS = 2)
try(data.table::setDTthreads(2))

# Force torch to install cpu-only version
Sys.setenv(CUDA = "cpu")
Sys.setenv(CUDA_VISIBLE_DEVICES = "")
# Avoid reticulate using uv for ephemeral environments
# Causes issues when hundreds of jobs create independent ephemeral envs simultaneously
# Better to use the One True Env in ./venv
# via uv, see also https://rstudio.github.io/reticulate/reference/py_require.html
Sys.setenv(RETICULATE_USE_MANAGED_VENV = "no")
# Force reticulate to use the project venv, ignoring PATH (e.g. spack's Python).
# RETICULATE_PYTHON overrides use_virtualenv(), so it has to agree with wherever
# the environment actually is: UV_PROJECT_ENVIRONMENT (uv's own variable) or
# .venv. That matters when the project directory is shared between machines
# resolving different interpreters -- a yolobox and its host, say, where each
# side's `uv sync` rebuilds .venv for itself and leaves the other pointing at an
# interpreter that does not exist. Set UV_PROJECT_ENVIRONMENT on one side and uv,
# reticulate, and R/helpers-python.R all follow it.
Sys.setenv(
	RETICULATE_PYTHON = file.path(
		getwd(),
		Sys.getenv("UV_PROJECT_ENVIRONMENT", unset = ".venv"),
		"bin",
		"python"
	)
)
# Unset PYTHONPATH to prevent spack's Python packages from interfering
Sys.unsetenv("PYTHONPATH")

if (requireNamespace("mlr3")) {
	lgr::get_logger("mlr3")$set_threshold("warn")
}
