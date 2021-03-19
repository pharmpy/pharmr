build_docs <- function() {
  modeling <- reticulate::import("pharmpy.modeling")
  funcs <- modeling$`__all__`
  
  py_capture_output <- reticulate:::py_capture_output
  import_builtins <- reticulate::import_builtins
  
  for (func in funcs) {
    py_original <- py_capture_output(import_builtins()$help(get(func)), type = "stdout")
    py_subbed_macros <- sub_latex_macros(py_original)
    py_split <- unlist(strsplit(py_subbed_macros, split='\n'))[-1]
    py_trimmed <- lapply(py_split, trimws)
    py_doc <- py_trimmed[py_trimmed != '']

    arguments <- c('')
    value <- c('')
    details <- py_doc[-1]
    argument_pos <- find_argument_position(py_doc)
    value_pos <- find_value_position(py_doc)

    if (!is.na(argument_pos$start)){
      arguments <- py_doc[argument_pos$start:argument_pos$end]
      arguments <- split_to_items(arguments)
      details <- py_doc[2:(argument_pos$start-3)]
    }
    if (!is.na(value_pos$start)) {
      value <- py_doc[value_pos$start:value_pos$end]
      if (is.na(argument_pos$start)){
        details <- py_doc[2:(value_pos$start-3)]
      }
    }

    description <- unlist(strsplit(as.character(details[1]), '\\. '))[1]
    details <- paste(details, collapse = '\n\n')

    value <- paste(value, collapse = '\n')
    file_name <- paste(getwd(), '/man/', func, '.Rd', sep = '')
    
    doc_link <- 'https://pharmpy.github.io/latest/reference/pharmpy.modeling.html'  # Add full link, underfull \hbox error (spaces are added)
    
    rd_content <- paste('\\name{', func, '}\n',
                        '\\alias{', func, '}\n',
                        '\\title{', func, '}\n',
                        '\\description{\n', description, '.\n\n',
                        '\\bold{Usage}\n\n\\code{', py_doc[1], '}\n', '\n}\n',
                        '\\details{\n',
                        'Link to Python\n\\href{', doc_link, '}{API reference} (for correct rendering of equations, tables etc.).\n',
                        details,'\n}\n',
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

sub_latex_macros <- function(doc_orignal) {
  doc_subbed <- gsub('\\\\mathsf', '', doc_orignal) %>%
    gsub('\\cdot', '*', .) %>%
    gsub('\\\\text', '', .) %>%
    gsub('\\\\frac', 'frac', .) %>%
    gsub('\\\\log', 'log', .) %>%
    gsub('\\\\exp', 'exp', .) %>%
    gsub('\\\\min', 'min', .) %>%
    gsub('\\\\max', 'max', .)

  return(doc_subbed)
}

find_argument_position <- function(full_doc) {
  start_index <- match('Parameters', full_doc) + 2
  if (is.na(start_index)) {
    return(list(start = NA, end = NA))
  }
  if ('Returns' %in% full_doc) {
    end_index <- match('Returns', full_doc) - 1
  }
  else {
    end_index <- length(full_doc)
  }
  return(list(start = start_index, end = end_index))
}

find_value_position <- function(full_doc) {
  start_index <- match('Returns', full_doc) + 2
  end_index <- length(full_doc)
  return(list(start = start_index, end = end_index))
}

split_to_items <- function(arguments_raw) {
  list_of_items <- grep(':', arguments_raw, value = TRUE)
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

library(magrittr)
library(pharmr)
build_docs()
