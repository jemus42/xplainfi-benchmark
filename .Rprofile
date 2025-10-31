# Ensure ranger behaves, particularly important for nested parallelization here with conditional sampling depending on ranger as well
options(ranger.num.threads = 1)
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OMP_THREAD_LIMIT = 1)
Sys.setenv(MKL_NUM_THREADS = 1)
try(data.table::setDTthreads(1))
