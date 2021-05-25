\name{additive_error}
\alias{additive_error}
\title{additive_error}
\description{
Set an additive error model.

\bold{Usage}

\code{additive_error(model, data_trans=NULL)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.additive_error}{API reference} (for correct rendering of equations, tables etc.).
Set an additive error model. Initial estimate for new sigma is :math:`(min(DV)/2)²`.

The error function being applied depends on the data transformation.

+------------------------+----------------------------------------+

| Data transformation    | Additive error                         |

+========================+========================================+

| :math:`y`              | :math:`f + epsilon_1`                 |

+------------------------+----------------------------------------+

| :math:`log(y)`         | :math:`log(f) + frac{epsilon_1}{f}` |

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
