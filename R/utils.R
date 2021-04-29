reset_index <- function(df) {
    ind <- attributes(df)$pandas.index
    inddf <- suppressWarnings(ind$to_frame(index=FALSE))
    inddf <- reticulate::py_to_r(inddf)
    cbind(inddf, df)
}
