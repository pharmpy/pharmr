.onLoad <- function(libname, pkgname) {
    if (reticulate::py_module_available("pharmpy")) {
        reticulate::import("pharmpy")

        env <- parent.env(environment())
        modeling <- reticulate::import("pharmpy.modeling")

        # Assign all functions in pharmpy.modeling to the parent environment
        funcs <- modeling$`__all__`
        for (func in funcs) {
            assign(func, modeling[[func]], env)
        }

        pharmpy_results <- reticulate::import("pharmpy.results")
        assign("read_results", pharmpy_results$read_results, env)
    }
}
