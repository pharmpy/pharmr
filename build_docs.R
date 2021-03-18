build_docs <- function() {
  modeling <- reticulate::import("pharmpy.modeling")
  funcs <- modeling$`__all__`
  
  py_capture_output <- reticulate:::py_capture_output
  import_builtins <- reticulate::import_builtins
  
  for (func in funcs) {
    py_doc <- py_capture_output(import_builtins()$help(get(func)), type = "stdout")
    py_doc <- gsub('\\\\mathsf', '', py_doc)
    py_doc <- gsub('\\cdot', '*', py_doc)
    py_doc <- gsub('\\\\text', '', py_doc)
    py_doc <- gsub('\\\\frac', 'frac', py_doc)
    py_doc <- gsub('\\\\log', 'log', py_doc)
    py_doc <- gsub('\\\\exp', 'exp', py_doc)
    py_doc <- gsub('\\\\min', 'min', py_doc)
    py_doc <- gsub('\\\\max', 'max', py_doc)
    
    py_doc <- unlist(strsplit(py_doc, split='\n'))[-1]
    py_doc <- lapply(py_doc, trimws)
    py_doc <- py_doc[py_doc != '']

    arguments <- c('')
    value <- c('')
    description <- py_doc[-1]
    
    if ('Parameters' %in% py_doc) {
      parameters_start_index <- match('Parameters', py_doc)
      arguments <- py_doc[-1:-(parameters_start_index + 1)]
      description <- py_doc[2:(parameters_start_index - 1)]
      if ('Returns' %in% py_doc) {
        returns_start_index <- match('Returns', py_doc)
        value <- py_doc[-1:-(returns_start_index + 1)]
        arguments <- arguments[1:(length(arguments) - length(value) - 2)]
      }
      arguments <- split_to_items(arguments)
    }
    
    description <- paste(description, collapse = '\n\n')
    
    value <- paste(value, collapse = '\n\n')
    file_name <- paste(getwd(), '/man/', func, '.Rd', sep = '')
    
    doc_link <- 'https://pharmpy.github.io/latest/reference/pharmpy.modeling.html?highlight=add_cov#pharmpy.modeling.'
    
    rd_content <- paste('\\name{', func, '}\n',
                        '\\alias{', func, '}\n',
                        '\\title{', func, '}\n',
                        '\\description{\n', 
                        '\\href{', doc_link, func, '}{Link to Python API reference} (for correct rendering of equations, tables etc.)\n\n',
                        description, 
                        '\n\n\\bold{Usage}\n\n\\code{', py_doc[1], '}\n', '\n}\n', 
                        sep = '')
    
    if ('Parameters' %in% py_doc) {
      rd_content <- paste(rd_content,
                          '\\arguments{\n', arguments, '\n}\n',
                          sep = '')
    }
    if ('Returns' %in% py_doc) {
      rd_content <- paste(rd_content,
                          '\\value{\n', value, '\n}\n',
                          sep = '')
    }
    
    rd_file <- cat(rd_content, file = file_name)
  }
}

split_to_items <- function(arguments_raw) {
  list_of_items <- grep(' : ', arguments_raw, value = TRUE)
  arguments <- ''
  
  for (def in list_of_items) {
    split <- unlist(strsplit(def, ':'))
    item <- trimws(split[1])
    type <- trimws(split[2])
    
    definition_index <- match(def, arguments_raw) + 1
    definition <- paste(type, trimws(arguments_raw[definition_index]), sep = ', ')
    arguments <- paste(arguments, '\\item{', item, '}{', definition, '}', sep = '\n')
  }
  return(arguments)
}

library(pharmr)
build_docs()
