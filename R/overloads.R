#' @exportS3Method length pharmpy.model.parameters.Parameters
length.pharmpy.model.parameters.Parameters <- function(obj, ...) {
    reticulate::py_len(obj)
}
