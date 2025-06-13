% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{run_ruvsearch}
\alias{run_ruvsearch}
\title{run_ruvsearch}
\usage{
run_ruvsearch(
  model,
  results,
  groups = 4,
  p_value = 0.001,
  skip = NULL,
  max_iter = 3,
  dv = NULL,
  strictness = "minimization_successful or (rounding_errors and sigdigs>=0.1)",
  ...
)
}
\arguments{
\item{model}{(Model) Pharmpy model}

\item{results}{(ModelfitResults) Results of model}

\item{groups}{(numeric) The number of bins to use for the time varying models}

\item{p_value}{(numeric) The p-value to use for the likelihood ratio test}

\item{skip}{(array(str) (optional)) A vector of models to not attempt.}

\item{max_iter}{(numeric) Number of iterations to run (1, 2, or 3). For models with BLQ only one iteration is supported.}

\item{dv}{(numeric (optional)) Which DV to assess the error model for.}

\item{strictness}{(str) Strictness criteri}

\item{...}{Arguments to pass to tool}
}
\value{
(RUVSearchResults) Ruvsearch tool result object
}
\description{
Run the ruvsearch tool. For more details, see :ref:\code{ruvsearch}.
}
\examples{
\dontrun{
model <- load_example_model("pheno")
results <- load_example_modelfit_results("pheno")
run_ruvsearch(model=model, results=results)
}

}
