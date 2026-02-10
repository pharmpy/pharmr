#' @exportS3Method length pharmpy.model.parameters.Parameters
length.pharmpy.model.parameters.Parameters <- function(x) {
    reticulate::py_len(x)
}

#' @exportS3Method py_to_r pandas.DataFrame
py_to_r.pandas.DataFrame <- reticulate:::py_to_r.pandas.core.frame.DataFrame

#' @exportS3Method py_to_r pandas.Series
py_to_r.pandas.Series <- reticulate:::py_to_r.pandas.core.series.Series
