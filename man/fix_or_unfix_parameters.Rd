% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{fix_or_unfix_parameters}
\alias{fix_or_unfix_parameters}
\title{fix_or_unfix_parameters}
\usage{
fix_or_unfix_parameters(model, parameters, strict = TRUE)
}
\arguments{
\item{model}{(Model) Pharmpy model}

\item{parameters}{(list(str=logical)) Set fix/unfix for these parameters}

\item{strict}{(logical) Whether all parameters in input need to exist in the model. Default is TRUE}
}
\value{
(Model) Pharmpy model object
}
\description{
Fix or unfix parameters

Set fixedness of parameters to specified values
}
\examples{
\dontrun{
model <- load_example_model("pheno")
model$parameters['POP_CL']
model <- fix_or_unfix_parameters(model, list('POP_CL'=TRUE))
model$parameters['POP_CL']
}
}
\seealso{
fix_parameters : Fix parameters

unfix_paramaters : Unfixing parameters

fix_paramaters_to : Fixing parameters and setting a new initial estimate in the same

function

unfix_paramaters_to : Unfixing parameters and setting a new initial estimate in the same

function
}
