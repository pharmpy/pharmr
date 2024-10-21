#' @exportS3Method length pharmpy.model.parameters.Parameters
length.pharmpy.model.parameters.Parameters <- function(x) {
    reticulate::py_len(x)
}
