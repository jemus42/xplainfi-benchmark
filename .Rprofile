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
# Force reticulate to use ephemeral enviornment
# via uv, see also https://rstudio.github.io/reticulate/reference/py_require.html
Sys.setenv(RETICULATE_PYTHON = "managed")
