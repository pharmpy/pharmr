.onLoad <- function(libname, pkgname) {
    if (reticulate::py_module_available("pharmpy")) {
        check_pharmpy()
        reticulate::import("pharmpy", delay_load=TRUE)

        env <- parent.env(environment())
        modeling <- reticulate::import("pharmpy.modeling", delay_load=TRUE)

        # Assign all functions in pharmpy.modeling to the parent environment
        funcs <- modeling$`__all__`
        for (func in funcs) {
            assign(func, modeling[[func]], env)
        }
    }
}
