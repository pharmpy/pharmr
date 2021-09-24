update_description <- function(release_version='minor') {
  pharmpy <- reticulate::import("pharmpy")
  pharmpy_version <- pharmpy$`__version__`
  
  desc::desc_set('Version', pharmpy_version)
  desc::desc_set('Date', Sys.Date())
}
library(desc)
library(here)
library(pharmr)

update_description()
