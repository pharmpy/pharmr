#' @title
#' Reset result indices
#' 
#' @description
#' Resets indices in dataframes within Results-objects when needed 
#' 
#' @param res A Pharmpy results object
reset_indices_results <- function(res) {
  attrs_new <- list()
  args_new <- c()
  for (py_attr_name in reticulate::py_list_attributes(res)) {
    # Skip e.g. __class__
    if (startsWith(py_attr_name, '_')) {
      next
    }
    py_attr <- reticulate::py_get_attr(res, py_attr_name)
    py_attr_class <- class(py_attr)
    
    # Skip object methods
    if ('python.builtin.method' %in% py_attr_class) {
      next
    }
    if ('pandas.core.frame.DataFrame' %in% py_attr_class) {
      # Reset index if dataframe has multiindex
      if (py_attr$index$nlevels > 1) {
        py_attr <- py_attr$reset_index()
      }
    }
    
    attrs_new[[py_attr_name]] <- py_attr
    # The following two lines creates a string that looks something like:
    #  summary_tool=attrs_new$summary_tool
    list_accessor <- paste('attrs_new', py_attr_name, sep='$')
    args_new <- c(args_new, paste(py_attr_name, list_accessor, sep='='))
  }
  
  # Combine all arguments into a comma separated string
  input_args <- paste(args_new, collapse=', ')
  # Get class name and transform to R, e.g. 
  #  pharmpy.tools$modelsearch$tool$ModelSearchResults
  res_class <- class(res)[1]
  res_class_r <- gsub('\\.', '$', res_class)
  # Create full constructor call, e.g.
  #  pharmpy.tools$modelsearch$tool$ModelSearchResults(summary_tool=attrs_new$summary_tool, ...)
  constructor_str <- paste(res_class_r, '(', input_args, ')', sep = '')
  res_new <- eval(parse(text=constructor_str))
  return(res_new)
}