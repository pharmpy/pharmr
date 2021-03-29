check_pharmpy <- function() {
  pharmpy <- reticulate::import("pharmpy")
  pharmpy_version <- pharmpy$`__version__`
  pharmr_version <- as.character(utils::packageVersion("pharmr"))
  
  pharmpy_major <- unlist(strsplit(pharmpy_version, '\\.'))[1]
  pharmpy_minor <- unlist(strsplit(pharmpy_version, '\\.'))[2]
  
  pharmr_major <- unlist(strsplit(pharmr_version, '\\.'))[1]
  pharmr_minor <- unlist(strsplit(pharmr_version, '\\.'))[2]
  
  if ((pharmpy_major != pharmr_major) | (pharmpy_minor != pharmr_minor)) {
    warning_str <- paste('Different versions of pharmpy ', 
                         '(', pharmpy_major, '.', pharmpy_minor, '.x) ',
                         'and pharmr (', pharmr_major, '.', pharmr_minor, '.x). ',
                         'Note that automatically generated documentation might ',
                         'be out of date.', sep='')
    warning(warning_str)
  }
}