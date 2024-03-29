% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{list_time_varying_covariates}
\alias{list_time_varying_covariates}
\title{list_time_varying_covariates}
\usage{
list_time_varying_covariates(model)
}
\arguments{
\item{model}{(Model) Pharmpy model}
}
\value{
(vector) Names of all time varying covariates
}
\description{
Return a vector of names of all time varying covariates
}
\examples{
\dontrun{
model <- load_example_model("pheno")
list_time_varying_covariates(model)
}
}
\seealso{
get_covariate_baselines : get baselines for all covariates
}
