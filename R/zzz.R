.onLoad <- function(libname, pkgname) {
    env <- parent.env(environment())
    pharmpy <- reticulate::import("pharmpy")
    modeling <- reticulate::import("pharmpy.modeling")

    # Assign all functions in pharmpy.modeling to the parent environment
    funcs <- modeling$`__all__`
    for (func in funcs) {
        assign(func, modeling[[func]], env)
    }

    pharmpy_results <- reticulate::import("pharmpy.results")
    read_results <- pharmpy_results$read_results
}
