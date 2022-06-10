#' @title
#' Install Pharmpy
#' 
#' @description
#' Install the pharmpy-core python package into virtual environment. Uses the same Pharmpy version as pharmr.
#' 
#' @param envname (str) name of environment. Default is r-reticulate
#' @param method (str) type of environment type (virtualenv, conda). Default is auto (virtualenv is not available on Windows)
#' 
#' @export
install_pharmpy <- function(envname='r-reticulate', method='auto') {
  pharmr:::install_pharmpy_devel(envname = envname, method = method, version = 'same')
}


#' @title
#' Install Pharmpy (with specified version)
#' 
#' @description
#' Install the pharmpy-core python package into virtual environment. 
#' 
#' @param envname (str) name of environment. Default is r-reticulate
#' @param method (str) type of environment type (virtualenv, conda). Default is auto (virtualenv is not available on Windows)
#' @param version (str) which pharmpy version to use (use 'same' for most cases)
#' 
#' @importFrom utils packageVersion
install_pharmpy_devel <- function(envname='r-reticulate', method='auto', version='same') {
  if (version == 'latest') {
    pharmpy_to_install <- 'pharmpy-core'
  }
  else { 
    if (version == 'same') {
      version <- packageVersion('pharmr')
    }
    pharmpy_to_install <- paste('pharmpy-core', version, sep='==')
  }
  reticulate::py_install(pharmpy_to_install, envname=envname, method=method, pip=T)
}

