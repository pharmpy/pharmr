convert_output <- function(obj) {
    if (inherits(obj, "pandas.DataFrame")
            || inherits(obj, "pandas.core.frame.DataFrame")
            || inherits(obj, "pandas.Series")
            || inherits(obj, "pandas.core.series.Series")) {
        index <- obj$index
        if (inherits(index, "pandas.RangeIndex")) {
            new_index <- index$`__class__`(index$start - 1, index$stop - 1, index$step)
            obj <- obj$set_axis(new_index)
        } else {
            nlevels <- as.integer(as.character(obj$index$nlevels)) # nlevels is of environment type
            if (nlevels > 1) {
                index_names <- obj$index$names
                obj <- obj$reset_index()
                conv <- reticulate::py_to_r(obj)
                attr(conv, "pandas.multiindex") <- reticulate::py_to_r(reticulate::import_builtins()$list(index_names))
                return(conv)
            }
        }
    }
    reticulate::py_to_r(obj)
}


is_named_onedim <- function(x) {
    is_onedim_array <- is.array(x) && (length(dim(x)) == 1)
    is_named <- !is.null(names(x))
    (is.vector(x) || is_onedim_array) && is_named
}


named_onedim_to_list <- function(x) {
    split(unname(x), names(x))
}


to_list <- function(x) {
    if (is.list(x)) {
        x
    } else if (is_named_onedim(x)) {
        named_onedim_to_list(x)
    } else {
        stop("Cannot convert to list")
    }
}


is_consecutive <- function(x) {
    all(diff(x) == 1)
}


create_integer_index <- function(rows) {
    pd <- reticulate::import("pandas", convert=FALSE)
    if (is_consecutive(rows)) {
        first <- rows[1]
        last <- utils::tail(rows, n=1)
        pd$RangeIndex(first, last + 1L, 1L)
    } else {
        rows
    }
}


backconvert_multiindex <- function(robj, pyobj, want) {
    # want is either "pd.Series" or "pd.DataFrame"
    multi_index <- attr(robj, "pandas.multiindex")
    if (!is.null(multi_index)) {
        if (all(multi_index %in% names(robj))) {
            pyobj <- pyobj$set_index(multi_index)
            if (want == "pd.Series") {
                if (reticulate::py_len(pyobj$columns) == 1) {
                    pyobj <- pyobj$squeeze()
                } else {
                    stop("Input has to many columns")
                }
            }
            return(pyobj)
        } else {
            stop("Bad multi index in data.frame")
        }
    }
}


convert_input <- function(arg, to_py_type) {
    if (is.null(arg)) {
        return(arg)
    } else if (to_py_type == 'Mapping') {
        return(to_list(arg))
    } else if (to_py_type == 'list') {
        return(as.list(arg))
    } else if (to_py_type == 'int') {
        if (is.numeric(arg)) {
            return(as.integer(arg))
        }
        return(arg)
    } else if (to_py_type %in% c('pd.DataFrame', 'pd.Series')) {
        if (inherits(arg, "data.frame")) {
            rows <- attr(arg, "row.names")
            if (length(rows) > 0) {
                df <- reticulate::r_to_py(arg)
                df2 <- backconvert_multiindex(arg, df, to_py_type)
                if (!is.null(df2)) {
                    return(df2)
                }
                old_index <- attr(arg, "pandas.index")
                is_old <- "pandas.RangeIndex" %in% class(old_index)
                is_df <- "pandas.RangeIndex" %in% class(df$index)
                if (is_old && is_df
                        && reticulate::py_to_r(old_index$start) == reticulate::py_to_r(df$index$start)
                        && reticulate::py_to_r(old_index$stop) ==  reticulate::py_to_r(df$index$stop)) {
                    index <- old_index$`__class__`(reticulate::py_to_r(old_index$start) + 1, reticulate::py_to_r(old_index$stop) + 1, reticulate::py_to_r(old_index$step))
                } else {
                    if (is.integer(rows)) {
                        index <- create_integer_index(rows)
                    } else {
                        int_rows <- suppressWarnings(as.integer(rows))
                        if (!anyNA(int_rows)) {
                            index <- create_integer_index(int_rows)
                        } else {
                            index <- rows
                        }
                    }
                }
                df <- df$set_axis(index)
                return(df)
            } else {
                return(arg)
            }
        } else if (to_py_type == 'pd.Series') {
            pd <- reticulate::import("pandas", convert=FALSE)
            return(pd$Series(to_list(arg)))
        }
    }
    arg
}
