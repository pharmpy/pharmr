% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{get_covariate_effects}
\alias{get_covariate_effects}
\title{get_covariate_effects}
\usage{
get_covariate_effects(model)
}
\arguments{
\item{model}{(Model) Model to extract covariates from.}
}
\value{
(Dictionary : Dictionary of parameters and connected covariate(s))
}
\description{
Return a list of all used covariates within a model

The list will have parameter name as key with a connected value as
a vector of tuple(s) with (covariate, effect type, operator)
}
