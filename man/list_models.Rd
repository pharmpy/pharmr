% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{list_models}
\alias{list_models}
\title{list_models}
\usage{
list_models(context, recursive = FALSE)
}
\arguments{
\item{context}{(Context) The context}

\item{recursive}{(logical) Only top level or all levels recursively down.}
}
\value{
(vectorc(str)) A vector of the model names
}
\description{
List names of all models in a context

Will by default vector only models in the top level, but can vector
all recursively using the recursive option. This will add the context
path to each model name as a qualifier.
}
