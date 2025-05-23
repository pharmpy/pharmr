% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install.R
\name{install_pharmpy_devel}
\alias{install_pharmpy_devel}
\title{Install Pharmpy (with specified version)}
\usage{
install_pharmpy_devel(
  envname = "r-reticulate",
  method = "auto",
  version = "devel"
)
}
\arguments{
\item{envname}{(str) name of environment. Default is r-reticulate}

\item{method}{(str) type of environment type (virtualenv, conda). Default is auto (virtualenv is not available on Windows)}

\item{version}{(str) which pharmpy version to use (use 'same' for most cases)}
}
\description{
Install the pharmpy-core python package into virtual environment.
}
