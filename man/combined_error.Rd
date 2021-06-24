\name{combined_error}
\alias{combined_error}
\title{combined_error}
\description{
Set a combined error model.

\bold{Usage}

\code{combined_error(model, data_trans=NULL)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.combined_error}{API reference} (for correct rendering of equations, tables etc.).
Set a combined error model. Initial estimates for new sigmas are :math:`(min(DV)/2)²` for

proportional and 0.09 for additive.

The error function being applied depends on the data transformation.

+------------------------+-----------------------------------------------------+

| Data transformation    | Combined error                                      |

+========================+=====================================================+

| :math:`y`              | :math:`f + f epsilon_1 + epsilon_2`               |

+------------------------+-----------------------------------------------------+

| :math:`log(y)`         | :math:`log(f) + epsilon_1 + frac{epsilon_2}{f}` |

+------------------------+-----------------------------------------------------+

This documentation was automatically generated from Pharmpy (0.25.0).}
\arguments{

\item{
model
}{
Model. Set error model for this model
}
\item{
data_trans
}{
character or expression. A data transformation expression or NULL (default) to use the transformation specified by the model.
}
}
