#' @title
#' Checks version of Pharmpy/pharmr 
#' 
#' @description
#' Checks whether Pharmpy and pharmr has the same version
#' 
#' @param pharmpy_version (str) version number as string
check_pharmpy <- function(pharmpy_version) {
  pharmr_version <- as.character(utils::packageVersion("pharmr"))
  
  pharmpy_major <- unlist(strsplit(pharmpy_version, '\\.'))[1]
  pharmpy_minor <- unlist(strsplit(pharmpy_version, '\\.'))[2]
  
  pharmr_major <- unlist(strsplit(pharmr_version, '\\.'))[1]
  pharmr_minor <- unlist(strsplit(pharmr_version, '\\.'))[2]
  
  if ((pharmpy_major != pharmr_major) | (pharmpy_minor != pharmr_minor)) {
    warning('Different versions of pharmpy ', 
            '(', pharmpy_major, '.', pharmpy_minor, '.x) ',
            'and pharmr (', pharmr_major, '.', pharmr_minor, '.x). ',
            'Note that automatically generated documentation might ',
            'be out of date.')
  }
}