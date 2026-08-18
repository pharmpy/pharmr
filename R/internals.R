convert_output <- function(obj) {
    if (inherits(obj, "pandas.DataFrame")
            || inherits(obj, "pandas.core.frame.DataFrame")
            || inherits(obj, "pandas.Series")
            || inherits(obj, "pandas.core.series.Series")) {
        index <- obj$index
        # FIXME: some version of reticulate started to shift the index 
        if (inherits(index, "pandas.RangeIndex")) {
            new_index <- index$`__class__`(index$start - 1, index$stop - 1, index$step)
            obj <- obj$set_axis(new_index)
        } else {
            nlevels <- as.integer(as.character(obj$index$nlevels)) # nlevels is of environment type
            if (nlevels > 1) {
                obj <- obj$reset_index()
            }
        }
    }
    return(obj)
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

convert_input <- function(arg, to_py_type) {
    if (is.null(arg)) {
        return(arg)
    }
    else if (to_py_type == 'Mapping') {
        return(to_list(arg))
    }
    else if (to_py_type == 'pd.Series') {
        pd <- reticulate::import("pandas", convert=FALSE)
        return(pd$Series(to_list(arg)))
    }
    else if (to_py_type == 'list') {
        return(as.list(arg))
    }
    else if (to_py_type == 'int') {
        if (is.numeric(arg)) {
            return(as.integer(arg))
        }
        return(arg)
    }
    else if (to_py_type == 'pd.DataFrame') {
        row_names <- row.names(arg)
        if (length(row_names) > 0 && !any(is.na(as.integer(row_names)))) {
            df <- reticulate::r_to_py(arg)
            old_index <- attr(arg, "pandas.index")
            is_old <- "pandas.RangeIndex" %in% class(old_index)
            is_df <- "pandas.RangeIndex" %in% class(df$index)
            if (is_old && is_df
                    && reticulate::py_to_r(old_index$start) == reticulate::py_to_r(df$index$start)
                    && reticulate::py_to_r(old_index$stop) ==  reticulate::py_to_r(df$index$stop)) {
                new_index <- old_index$`__class__`(reticulate::py_to_r(old_index$start) + 1, reticulate::py_to_r(old_index$stop) + 1, reticulate::py_to_r(old_index$step))
                df <- df$set_axis(new_index)
            } else {
                df <- df$set_axis(df$index$astype("int"))
            }
            return(df)
        } else {
            return(arg)
        }
    }
}
