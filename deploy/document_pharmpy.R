library(reticulate)
library(devtools)

reticulate::py_run_file('build_functions.py')
devtools::document()