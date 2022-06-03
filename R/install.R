#' @title
#' Install Pharmpy
#' 
#' @description
#' Install the pharmpy-core python package into virtual environment. Uses the same Pharmpy version as pharmr.
#' 
#' @param envname (str) name of environment. Default is r-reticulate
#' @param method (str) type of environment type (virtualenv, conda). Default is auto (virtualenv is not available on Windows)
#' 
#' @importFrom utils packageVersion
#' @export
install_pharmpy <- function(envname='r-reticulate', method='auto') {
    version <- packageVersion('pharmr')
    pharmpy_to_install <- paste('pharmpy-core', version, sep='==')
    reticulate::py_install(pharmpy_to_install, envname=envname, method=method, pip=T)
}
