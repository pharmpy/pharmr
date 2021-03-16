\name{add_covariate_effect}
\alias{add_covariate_effect}
\title{add_covariate_effect}
\description{
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html?highlight=add_cov#pharmpy.modeling.add_covariate_effect}{Link to Python API reference} (e.g. for correct rendering of equations)

Adds covariate effect to :class:`pharmpy.model`. The following effects have templates:

- Linear function for continuous covariates (*lin*)

- Initial estimate: 0.001

- Upper bound: 100,000 if the median of the covariate is equal to the minimum, otherwise

:math:`1/(median - min)`

- Lower bound: -100,000 if the median of the covariate is equal to the maximum, otherwise

:math:`1/(median - max)`

- Linear function for categorical covariates (*cat*)

- Initial estimate: 0.001

- Upper bound: 100,000

- Lower bound: -100,000

- Piecewise linear function/"hockey-stick", continuous covariates only (*piece_lin*)

- Initial estimate: 0.001

- Upper bound: for first state 1/(median - minimum), otherwise 100,000

- Lower bound: for first state -100,000, otherwise 1/(median - maximum)

- Exponential function, continuous covariates only (*exp*)

- Initial estimate: 0.001 unless lower bound > 0.001 or upper bound < 0.001. In that case

:math:`init = (upper - lower)/2`, if init = 0: :math:`init = upper/2`

- Upper bound: if :math:`min - median = 0` or :math:`max - median = 0`, upper bound is 100.

Otherwise the upper bound is

:math:`min(log(0.01)/(min - median), log(100)/(max - median))`

- Lower bound: if :math:`min - median = 0` or :math:`max - median = 0`, lower bound is 0.01.

Otherwise the lower bound is

:math:`max(log(0.01)/(max - median), log(100)/(min - median))`

- Power function, continuous covariates only (*pow*)

- Initial estimate: 0.001

- Upper bound: 100,000

- Lower bound: -100
}
\usage{
add_covariate_effect(model, parameter, covariate, effect, operation='*')
}
\arguments{

\item{
model
}{
Model, Pharmpy model to add covariate effect to.
}
\item{
parameter
}{
str, Name of parameter to add covariate effect to.
}
\item{
covariate
}{
str, Name of covariate.
}
\item{
effect
}{
str, Type of covariate effect. May be abbreviated covariate effect (see above) or custom.
}
\item{
operation
}{
str, optional, Whether the covariate effect should be added or multiplied (default).
}
}
\value{

}
