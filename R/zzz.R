.onLoad <- function(libname, pkgname) {
    if (reticulate::py_module_available("pharmpy")) {
        pharmpy <- reticulate::import("pharmpy", delay_load=TRUE)
        check_pharmpy(pharmpy$`__version__`)
        env <- parent.env(environment())

        # Assign all functions in pharmpy.modeling to the parent environment
        funcs <- pharmpy$modeling$`__all__`
        for (func in funcs) {
            assign(func, pharmpy$modeling[[func]], env)
        }
    }
}
