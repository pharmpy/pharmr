#' @exportS3Method "$" pharmpy.internals.immutable.Immutable
`$.pharmpy.internals.immutable.Immutable` <- function(x, name)
{
    obj <- reticulate::py_get_attr(x, name)
    obj <- convert_output(obj)
    reticulate::py_to_r(obj)
}

#' @exportS3Method length pharmpy.model.parameters.Parameters
length.pharmpy.model.parameters.Parameters <- function(x) {
    reticulate::py_len(x)
}
