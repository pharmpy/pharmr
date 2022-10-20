# The knit_print functions are used by knitr in rmarkdown
# The repr_html functions are used in jupyter

#' @exportS3Method knitr::knit_print pharmpy.model.statements.Assignment
knit_print.pharmpy.model.statements.Assignment <- function(x, ...) {
    res <- x$`_repr_latex_`()
    structure(res, class='knit_asis')
}

#' @exportS3Method knitr::knit_print pharmpy.model.statements.Statements
knit_print.pharmpy.model.statements.Statements <- function(x, ...) {
    res <- x$`_repr_html_`()
    structure(res, class='knit_asis')
}

#' @exportS3Method knitr::knit_print pharmpy.model.distributions.symbolic.NormalDistribution
knit_print.pharmpy.model.distributions.symbolic.NormalDistribution <- function(x, ...) {
    res <- x$`_repr_latex_`()
    structure(res, class='knit_asis')
}

#' @exportS3Method knitr::knit_print pharmpy.model.distributions.symbolic.JointNormalDistribution
knit_print.pharmpy.model.distributions.symbolic.JointNormalDistribution <- function(x, ...) {
    res <- x$`_repr_latex_`()
    structure(res, class='knit_asis')
}

#' @exportS3Method knitr::knit_print pharmpy.model.random_variables.RandomVariables
knit_print.pharmpy.model.random_variables.model.RandomVariables <- function(x, ...) {
    res <- x$`_repr_latex_`()
    structure(res, class='knit_asis')
}

#' @exportS3Method knitr::knit_print pharmpy.model.statements.ExplicitODESystem
knit_print.pharmpy.model.statements.ExplicitODESystem <- function(x, ...) {
    res <- x$`_repr_latex_`()
    structure(res, class='knit_asis')
}

#' @exportS3Method repr::repr_latex pharmpy.model.statements.Assignment
repr_latex.pharmpy.model.statements.Assignment <- function(obj, ...) {
    obj$`_repr_latex_`()
}

#' @exportS3Method repr::repr_html pharmpy.model.statements.Statements
repr_html.pharmpy.model.statements.Statements <- function(obj, ...) {
    obj$`_repr_html_`()
}

#' @exportS3Method repr::repr_latex pharmpy.model.statements.ExplicitODESystem
repr_latex.pharmpy.model.statements.ExplicitODESystem <- function(obj, ...) {
    obj$`_repr_latex_`()
}

#' @exportS3Method repr::repr_latex pharmpy.model.distributions.symbolic.NormalDistribution
repr_latex.pharmpy.model.distributions.symbolic.NormalDistribution <- function(obj, ...) {
    obj$`_repr_latex_`()
}

#' @exportS3Method repr::repr_latex pharmpy.model.distributions.symbolic.JointNormalDistribution
repr_latex.pharmpy.model.distributions.symbolic.JointNormalDistribution <- function(obj, ...) {
    obj$`_repr_latex_`()
}

#' @exportS3Method repr::repr_latex pharmpy.model.random_variables.RandomVariables
repr_latex.pharmpy.model.random_variables.RandomVariables <- function(obj, ...) {
    obj$`_repr_latex_`()
}
