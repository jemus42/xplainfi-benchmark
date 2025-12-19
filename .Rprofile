source("rv/scripts/rvr.R")
source("rv/scripts/activate.R")
# Ensure ranger behaves, particularly important for nested parallelization here with conditional sampling depending on ranger as well
options(ranger.num.threads = 1)
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OMP_THREAD_LIMIT = 1)
Sys.setenv(MKL_NUM_THREADS = 1)
try(data.table::setDTthreads(1))

# Force torch to install cpu-only version
Sys.setenv(CUDA = "cpu")
Sys.setenv(CUDA_VISIBLE_DEVICES = "")
# Avoid reticulate using uv for ephemeral environments
# Causes issues when hundreds of jobs create independent ephemeral envs simultaneously
# Better to use the One True Env in ./venv
# via uv, see also https://rstudio.github.io/reticulate/reference/py_require.html
Sys.setenv(RETICULATE_USE_MANAGED_VENV="no")
