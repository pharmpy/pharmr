#' @title
#' Install Pharmpy
#' 
#' @description
#' Install the pharmpy-core python package into virtual environment.
#' 
#' @param envname (str) name of environment. Default is r-reticulate
#' @param method (str) type of environment type (virtualenv, conda). Default is auto (virtualenv is not available on Windows)
#' 
#' @export
install_pharmpy <- function(envname='r-reticulate', method='auto') {
    reticulate::py_install("pharmpy-core", envname=envname, method=method, pip=FALSE)
}
