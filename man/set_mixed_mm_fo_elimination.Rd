% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{set_mixed_mm_fo_elimination}
\alias{set_mixed_mm_fo_elimination}
\title{set_mixed_mm_fo_elimination}
\usage{
set_mixed_mm_fo_elimination(model)
}
\arguments{
\item{model}{(Model) Pharmpy model}
}
\value{
(Model) Pharmpy model object
}
\description{
Sets elimination to mixed Michaelis-Menten and first order.

Initial estimate for CLMM is set to CL/2 and KM is set to (equation could not be rendered, see API doc on website)
}
\examples{
\dontrun{
model <- load_example_model("pheno")
model <- set_mixed_mm_fo_elimination(model)
model$statements$ode_system
}
}
\seealso{
set_first_order_elimination

set_zero_order_elimination

set_michaelis_menten_elimination
}
