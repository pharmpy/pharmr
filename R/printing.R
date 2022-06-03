# The knit_print functions are used by knitr in rmarkdown
# The repr_html functions are used in jupyter

#' @exportS3Method knitr::knit_print pharmpy.statements.Assignment
knit_print.pharmpy.statements.Assignment <- function(x, ...) {
    res <- x$`_repr_latex_`()
    structure(res, class='knit_asis')
}

#' @exportS3Method knitr::knit_print pharmpy.statements.ModelStatements
knit_print.pharmpy.statements.ModelStatements <- function(x, ...) {
    res <- x$`_repr_html_`()
    structure(res, class='knit_asis')
}

#' @exportS3Method knitr::knit_print pharmpy.random_variables.RandomVariable
knit_print.pharmpy.random_variables.RandomVariable <- function(x, ...) {
    res <- x$`_repr_latex_`()
    structure(res, class='knit_asis')
}

#' @exportS3Method knitr::knit_print pharmpy.random_variables.RandomVariables
knit_print.pharmpy.random_variables.RandomVariables <- function(x, ...) {
    res <- x$`_repr_latex_`()
    structure(res, class='knit_asis')
}

#' @exportS3Method repr::repr_latex pharmpy.statements.Assignment
repr_latex.pharmpy.statements.Assignment <- function(obj, ...) {
    obj$`_repr_latex_`()
}

#' @exportS3Method repr::repr_html pharmpy.statements.ModelStatements
repr_html.pharmpy.statements.ModelStatements <- function(obj, ...) {
    obj$`_repr_html_`()
}

#' @exportS3Method repr::repr_latex pharmpy.random_variables.RandomVariable
repr_latex.pharmpy.random_variables.RandomVariable <- function(obj, ...) {
    obj$`_repr_latex_`()
}

#' @exportS3Method repr::repr_latex pharmpy.random_variables.RandomVariables
repr_latex.pharmpy.random_variables.RandomVariables <- function(obj, ...) {
    obj$`_repr_latex_`()
}
