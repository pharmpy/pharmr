% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{set_lloq_data}
\alias{set_lloq_data}
\title{set_lloq_data}
\usage{
set_lloq_data(model, value, lloq = NULL, blq = NULL)
}
\arguments{
\item{model}{(Model) Pharmpy model object}

\item{value}{(str or numeric or Expr) The new dv value}

\item{lloq}{(numeric or str (optional)) Value or column name for lower limit of quantification.}

\item{blq}{(str (optional)) Column name for below limit of quantification indicator.}
}
\value{
(Model) Pharmpy model object
}
\description{
Set a dv value for lloq data records
}
\examples{
\dontrun{
model <- load_example_model("pheno")
model <- set_lloq_data(model, 0, lloq=10)
}
}
\seealso{
remove_loq_data

transform_blq
}
