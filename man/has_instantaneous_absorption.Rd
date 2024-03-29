% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{has_instantaneous_absorption}
\alias{has_instantaneous_absorption}
\title{has_instantaneous_absorption}
\usage{
has_instantaneous_absorption(model)
}
\arguments{
\item{model}{(Model) Pharmpy model}
}
\value{
(Bool : TRUE if model has instantaneous absorption)
}
\description{
Check if ode system describes a instantaneous absorption

Defined as being a instantaneous dose directly into the central compartment
}
