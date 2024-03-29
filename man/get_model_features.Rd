% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{get_model_features}
\alias{get_model_features}
\title{get_model_features}
\usage{
get_model_features(model, supress_warnings = FALSE)
}
\arguments{
\item{model}{(Model) Model to extract features from.}

\item{supress_warnings}{(logical) Choose to supress warnings if absorption/elimination type cannot be
determined. The default is FALSE.}
}
\value{
(str) A MFL string representation of the input model.
}
\description{
Create an MFL representation of an input model

Given an input model. Create a model feature language (MFL) string
representation. Can currently extract absorption, elimination, transits,
peripherals and lagtime.
}
