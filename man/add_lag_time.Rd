% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{add_lag_time}
\alias{add_lag_time}
\title{add_lag_time}
\usage{
add_lag_time(model)
}
\arguments{
\item{model}{(Model) Pharmpy model}
}
\value{
(Model) Pharmpy model object
}
\description{
Add lag time to the dose compartment of model.

Initial estimate for lag time is set the
previous lag time if available, otherwise it is set to the time of first observation/2.
}
\examples{
\dontrun{
model <- load_example_model("pheno")
model <- add_lag_time(model)
}
}
\seealso{
set_transit_compartments

remove_lag_time
}
