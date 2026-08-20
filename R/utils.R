#' @title
#' Reset index
#' 
#' @description
#' Reset index of dataframe. 
#' 
#' Reset index from a multi indexed data.frame or a non-standard index
#' @param df A data.frame coming from Pharmpy
#' @export
reset_index <- function(df) {
    attr(df, "pandas.multiindex") <- NULL
    attr(df, "row.names") <- NULL
    df
}

#' @title
#' Set index
#' 
#' @description
#' Set a multiindex for a dataframe. 
#' 
#' The index columns will be used when using the data.frame in a pharmpy call
#' @param df A data.frame
#' @param colnames An array of column names
#' @export
set_index <- function(df, colnames) {
    attr(df, "pandas.multiindex") <- colnames
    df
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
