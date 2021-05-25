\name{proportional_error}
\alias{proportional_error}
\title{proportional_error}
\description{
Set a proportional error model.

\bold{Usage}

\code{proportional_error(model, data_trans=NULL)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.proportional_error}{API reference} (for correct rendering of equations, tables etc.).
Set a proportional error model. Initial estimate for new sigma is 0.09.

The error function being applied depends on the data transformation.

+------------------------+----------------------------------------+

| Data transformation    | Proportional error                     |

+========================+========================================+

| :math:`y`              | :math:`f + f epsilon_1`               |

+------------------------+----------------------------------------+

| :math:`log(y)`         | :math:`log(f) + epsilon_1`           |

+------------------------+----------------------------------------+

This documentation was automatically generated from Pharmpy (0.24.2).}
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
