on_load_pharmpy <- function() {
    pharmpy_version <- py_to_r(pharmpy$`__version__`)
    check_pharmpy(pharmpy_version)
}

on_error_pharmpy <- function(e) {
    message("Error importing pharmpy")
    message("Check if pharmpy is installed")
    message("Install by pharmr::install_pharmpy() and reload library")
}

pharmpy <- NULL

#' @import vegawidget
#' @importFrom reticulate py_to_r
.onLoad <- function(libname, pkgname) {
    # This is to both support older reticulate versions and pandas 3
    reticulate_version <- utils::packageVersion("reticulate")
    if (reticulate_version <= "1.44.1") {
        reticulate_env <- asNamespace("reticulate")
        df_fn <- getFromNamespace("py_to_r.pandas.core.frame.DataFrame", "reticulate")
        ser_fn <- getFromNamespace("py_to_r.pandas.core.series.Series", "reticulate")
        registerS3method("py_to_r", "pandas.DataFrame", df_fn, envir=reticulate_env)
        registerS3method("py_to_r", "pandas.Series", ser_fn, envir=reticulate_env)
    }

    # Lazy loading cannot currently be avoided even for the case when pharmpy
    # is already installed.
    # This is because R attempts to load the package at installation time
    # Also testing to import by tryCatch breaks the delay_load fallback
    # because Python will be setup by reticulate and then lazy will be skipped
    # if Python was already setup.
    pharmpy <<- reticulate::import("pharmpy", convert=FALSE,
                                   delay_load=list(on_load=on_load_pharmpy,
                                                   on_error=on_error_pharmpy))
}
