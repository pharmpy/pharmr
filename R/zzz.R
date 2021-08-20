on_load_pharmpy <- function() {
    message("Loading pharmpy")
    check_pharmpy(pharmpy$`__version__`)
}

on_error_pharmpy <- function(e) {
    message("Error importing pharmpy")
    message("Check if pharmpy is installed")
    message("Install by pharmr::install_pharmpy() and reload library")
}

pharmpy <- NULL

.onLoad <- function(libname, pkgname) {
    pharmpy <<- reticulate::import("pharmpy", delay_load=list(on_load=on_load_pharmpy, 
                                                              on_error=on_error_pharmpy))
}