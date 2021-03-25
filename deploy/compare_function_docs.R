compare_function_docs <- function() {
  reticulate::import("pharmpy")
  
  modeling <- reticulate::import("pharmpy.modeling")
  pharmpy_funcs_new <- modeling$`__all__`
  
  pharmr_docs <- c('install_pharmpy.Rd', 'check_pharmpy.Rd')
  pharmpy_docs <- list.files(here('man'))
  
  pharmpy_funcs_old <- pharmpy_docs[!(pharmpy_docs %in% pharmr_docs)]
  
  if (length(pharmpy_funcs_new) > length(pharmpy_funcs_old)) {
    stop('All Pharmpy functions have not been accounted for in documentation.')
  }
  else if (length(pharmpy_funcs_new) < length(pharmpy_funcs_old)) {
    stop('Removed Pharmpy function still has documentation.')
  }
}

library(reticulate)
library(here)

compare_function_docs()