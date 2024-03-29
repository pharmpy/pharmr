% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{replace_non_random_rvs}
\alias{replace_non_random_rvs}
\title{replace_non_random_rvs}
\usage{
replace_non_random_rvs(model)
}
\arguments{
\item{model}{(Model) Pharmpy model}
}
\value{
(Model) A new model
}
\description{
Replace all random variables that are not actually random

Some random variables are constant. For example a normal
distribution with the variance parameter fixed to 0 will always
yield a single value when sampled. This function will find all such
random variables and replace them with their constant value in the model.
}
