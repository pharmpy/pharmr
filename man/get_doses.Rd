% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{get_doses}
\alias{get_doses}
\title{get_doses}
\usage{
get_doses(model)
}
\arguments{
\item{model}{(Model) Pharmpy model}
}
\value{
(data.frame) doses
}
\description{
Get a series of all doses

Indexed with ID and TIME
}
\examples{
\dontrun{
model <- load_example_model("pheno")
get_doses(model)
}

}
