% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{set_initial_estimates}
\alias{set_initial_estimates}
\title{set_initial_estimates}
\usage{
set_initial_estimates(
  model,
  inits,
  move_est_close_to_bounds = FALSE,
  strict = TRUE
)
}
\arguments{
\item{model}{(Model) Pharmpy model to update initial estimates}

\item{inits}{(list(str=numeric)) Initial parameter estimates to update}

\item{move_est_close_to_bounds}{(logical) Move estimates that are close to bounds. If correlation >0.99 the correlation will
be set to 0.9, if variance is <0.001 the variance will be set to 0.01.}

\item{strict}{(logical) Whether all parameters in input need to exist in the model. Default is TRUE}
}
\value{
(Model) Pharmpy model object
}
\description{
Update initial parameter estimate for a model

Updates initial estimates of population parameters for a model.
If the new initial estimates are out of bounds or NaN this function will raise.
}
\examples{
\dontrun{
model <- load_example_model("pheno")
results <- load_example_modelfit_results("pheno")
model$parameters$inits
model <- set_initial_estimates(model, results$parameter_estimates)
model$parameters$inits
model <- load_example_model("pheno")
model <- set_initial_estimates(model, list('POP_CL'=2.0))
model$parameters['POP_CL']
}
}
\seealso{
fix_parameters_to : Fixing and setting parameter initial estimates in the same function

unfix_paramaters_to : Unfixing parameters and setting a new initial estimate in the same
}
