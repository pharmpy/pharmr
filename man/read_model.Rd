% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{read_model}
\alias{read_model}
\title{read_model}
\usage{
read_model(path, missing_data_token = NULL)
}
\arguments{
\item{path}{(str) Path to model}

\item{missing_data_token}{(str (optional)) Use this token for missing data. This option will override the token from the config.
(This option was added in Pharmpy version 1.2.0)}
}
\value{
(Model) Read model object
}
\description{
Read model from file
}
\examples{
\dontrun{
model <- read_model("/home/run1$mod")
}
}
\seealso{
read_model_from_database : Read model from database

read_model_from_string : Read model from string
}
