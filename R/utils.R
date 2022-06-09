#' @title
#' Reset index
#' 
#' @description
#' Reset index of dataframe. 
#' 
#' Reset index from a multi indexed data.frame so that index is added as columns
#' @param df A data.frame converted from python using reticulate
#' @export
reset_index <- function(df) {
    ind <- attributes(df)$pandas.index
    inddf <- suppressWarnings(ind$to_frame(index=FALSE))
    inddf <- reticulate::py_to_r(inddf)
    cbind(inddf, df)
}


#' @title
#' Print pharmpy version
#' 
#' @description
#' Print the pharmpy version pharmr uses. 
#' 
#' @export
print_pharmpy_version <- function() {
  print(pharmpy$`__version__`)
}