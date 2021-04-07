build_docs <- function() {
  pharmpy <- reticulate::import("pharmpy")
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
    usage <- sub_python_args(py_doc[1])

    value <- paste(value, collapse = '\n')
    file_name <- paste(here('man', func), '.Rd', sep = '')
    
    doc_link <- paste('https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.', func,
                      sep='')
    
    rd_content <- paste('\\name{', func, '}\n',
                        '\\alias{', func, '}\n',
                        '\\title{', func, '}\n',
                        '\\description{\n', description, '.\n\n',
                        '\\bold{Usage}\n\n\\code{', usage, '}\n', '\n}\n',
                        '\\details{\n',
                        'Link to Python\n\\href{', doc_link, '}{API reference} (for correct rendering of equations, tables etc.).\n',
                        details,'\n\nThis documentation was automatically generated from Pharmpy (', pharmpy$`__version__`, ').}\n',
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
  doc_subbed <- str_replace_all(doc_orignal, c('\\\\mathsf'='',
                                               '\\\\cdot'='*', 
                                               '\\\\text'='', 
                                               '\\\\frac'='frac', 
                                               '\\\\log'='log', 
                                               '\\\\exp'='exp', 
                                               '\\\\min'='min', 
                                               '\\\\max'='max'))

  return(doc_subbed)
}

sub_python_args <- function(py_args) {
  r_args <- str_replace_all(py_args, c('list\\b'='vector', 
                                       'dict(ionary)*\\b'='list',
                                       'str\\b'='character', 
                                       'int\\b'='numeric',
                                       'None\\b'='NULL', 
                                       'True\\b'='TRUE',
                                       'False\\b'='FALSE'))
  
  return(r_args)
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

split_to_items <- function(args_raw) {
  args_split <- ''
  
  name_idx <- grep(':', args_raw)
  all_idx <- 1:length(args_raw)
  definition_idx <- all_idx %>%
    cbind(all=., intervals=findInterval(., name_idx)) %>%
    data.frame()

  for (i in unique(definition_idx[,'intervals'])) {
    arg_idx <- definition_idx %>%
      filter(intervals == i) %>%
      pull(all)
    
    item_start <- arg_idx[1]

    split <- unlist(strsplit(args_raw[[item_start]], ':'))
    item <- trimws(split[1])
    type <- trimws(split[2]) %>%
      sub_python_args()
    
    if (length(arg_idx) > 1) {
      description <- paste(args_raw[arg_idx][-1], collapse=' ')
      definition <- paste(type, description, sep = '. ')
    }
    else {
      definition <- type
    }
    args_split <- paste(args_split, '\\item{', item, '}{', definition, '}', sep = '\n')
  }
  
  return(args_split)
}

library(dplyr)
library(stringr)
library(here)
library(pharmr)

build_docs()
