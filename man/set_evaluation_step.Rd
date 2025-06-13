% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{set_evaluation_step}
\alias{set_evaluation_step}
\title{set_evaluation_step}
\usage{
set_evaluation_step(model, idx = -1)
}
\arguments{
\item{model}{(Model) Pharmpy model}

\item{idx}{(numeric) Index of estimation step, default is -1 (last estimation step)}
}
\value{
(Model) Pharmpy model object
}
\description{
Set evaluation step

Change the final or the estimation step with a specific index to do evaulation.
}
\examples{
\dontrun{
model <- load_example_model("pheno")
model <- set_evaluation_step(model)
model$execution_steps[1]
}
}
\seealso{
set_estimation_step

add_estimation_step

remove_estimation_step

append_estimation_step_options

add_parameter_uncertainty_step

remove_parameter_uncertainty_step
}
