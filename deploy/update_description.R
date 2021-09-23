update_description <- function(release_version='minor') {
  pharmpy <- reticulate::import("pharmpy")
  pharmpy_version <- pharmpy$`__version__`
  
  description <- paste("An R wrapper for the 'Pharmpy' 'pharmacometrics' library via the 'reticulate package'.",  
                       "The 'Pharmpy' documentation was automatically generated from 'Pharmpy' version", 
                       pharmpy_version, ".", sep=" ")
  
  desc::desc_set('Version', pharmpy_version)
  desc::desc_set('Date', Sys.Date())
  desc::desc_set('Description', description)
}
library(desc)
library(here)
library(pharmr)

update_description()
