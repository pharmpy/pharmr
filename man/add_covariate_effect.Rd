\name{add_covariate_effect}
\alias{add_covariate_effect}
\title{add_covariate_effect}
\description{
Adds covariate effect to :class:`pharmpy.model`.

\bold{Usage}

\code{add_covariate_effect(model, parameter, covariate, effect, operation='*')}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.add_covariate_effect}{API reference} (for correct rendering of equations, tables etc.).
Adds covariate effect to :class:`pharmpy.model`. The following effects have templates:

- Linear function for continuous covariates (*lin*)

- Function:

.. math::

{coveff} = 1 + {theta} * ({cov} - {median})

- Init:  0.001

- Upper:

- If median of covariate equals minimum: :math:`100,000`

- Otherwise: :math:`frac{1}{{median} - {min}}`

- Lower:

- If median of covariate equals maximum: :math:`-100,000`

- Otherwise: :math:`frac{1}{{median} - {max}}`

- Linear function for categorical covariates (*cat*)

- Function:

- If covariate is most common category:

.. math::

{coveff} = 1

- For each additional category:

.. math::

{coveff} = 1 + {theta}

- Init: :math:`0.001`

- Upper: :math:`100,000`

- Lower: :math:`-100,000`

- Piecewise linear function/"hockey-stick", continuous covariates only (*piece_lin*)

- Function:

- If cov <= median:

.. math::

{coveff} = 1 + {theta1} * ({cov} - {median})

- If cov > median:

.. math::

{coveff} = 1 + {theta2} * ({cov} - {median})

- Init: :math:`0.001`

- Upper:

- For first state: :math:`frac{1}{{median} - {min}}`

- Otherwise: :math:`100,000`

- Lower:

- For first state: :math:`-100,000`

- Otherwise: :math:`frac{1}{{median} - {max}}`

- Exponential function, continuous covariates only (*exp*)

- Function:

.. math::

{coveff} = exp({theta} * ({cov} - {median}))

- Init:

- If lower > 0.001 or upper < 0.001: :math:`frac{{upper} - {lower}}{2}`

- If estimated init is 0: :math:`frac{{upper}}{2}`

- Otherwise: :math:`0.001`

- Upper:

- If min - median = 0 or max - median = 0: :math:`100`

- Otherwise:

.. math::

min(frac{log(0.01)}{{min} - {median}},

frac{log(100)}{{max} - {median}})

- Lower:

- If min - median = 0 or max - median = 0: :math:`0.01`

- Otherwise:

.. math::

max(frac{log(0.01)}{{max} - {median}},

frac{log(100)}{{min} - {median}})

- Power function, continuous covariates only (*pow*)

- Function:

.. math::

{coveff} = (frac{{cov}}{{median}})^{theta}

- Init: :math:`0.001`

- Upper: :math:`100,000`

- Lower: :math:`-100`

This documentation was automatically generated from Pharmpy (0.25.0).}
\arguments{

\item{
model
}{
Model. Pharmpy model to add covariate effect to.
}
\item{
parameter
}{
character. Name of parameter to add covariate effect to.
}
\item{
covariate
}{
character. Name of covariate.
}
\item{
effect
}{
character. Type of covariate effect. May be abbreviated covariate effect (see above) or custom.
}
\item{
operation
}{
character, optional. Whether the covariate effect should be added or multiplied (default).
}
}
