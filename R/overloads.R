#' @exportS3Method "$" pharmpy.internals.immutable.Immutable
`$.pharmpy.internals.immutable.Immutable` <- function(x, name)
{
    obj <- reticulate::py_get_attr(x, name)
    convert_output(obj)
}

#' @exportS3Method length pharmpy.model.parameters.Parameters
length.pharmpy.model.parameters.Parameters <- function(x) {
    reticulate::py_len(x)
}
