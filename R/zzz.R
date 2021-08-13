on_load_pharmpy <- function() {
    check_pharmpy(pharmpy$`__version__`)
    env <- parent.env(environment())
    
    # Assign all functions in pharmpy.modeling to the parent environment
    funcs <- pharmpy$modeling$`__all__`
    for (func in funcs) {
        assign(func, pharmpy$modeling[[func]], env)
    }
}

on_error_pharmpy <- function() {
    message("Error importing pharmpy")
    message("Check if pharmpy is installed")
}

pharmpy <- NULL

.onLoad <- function(libname, pkgname) {
    if (reticulate::py_module_available("pharmpy")) {
        pharmpy <<- reticulate::import("pharmpy", delay_load=list(on_load=on_load_pharmpy, on_error=on_error_pharmpy))
    }
}