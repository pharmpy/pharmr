update_description <- function(release_version='minor') {
  desc::desc_bump_version(release_version)
  desc::desc_set('Date', Sys.Date())
}
library(desc)
library(here)

release_version <- commandArgs(trailingOnly = TRUE)

if (length(release_version) > 0) {
  update_description(release_version)
} else {
  update_description()
}