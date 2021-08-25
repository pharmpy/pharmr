#' @title
#' Install Pharmpy
#' 
#' @description
#' Install the pharmpy-core python package into the r-reticulate virtual environment
#' 
#' @export
install_pharmpy <- function() {
    reticulate::py_install("pharmpy-core", pip=T)
}
