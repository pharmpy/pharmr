\name{add_iiv}
\alias{add_iiv}
\title{add_iiv}
\description{
Adds IIVs to :class:`pharmpy.model`.

\bold{Usage}

\code{add_iiv(model, list_of_parameters, expression, operation='*', eta_names=NULL)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.add_iiv}{API reference} (for correct rendering of equations, tables etc.).
Adds IIVs to :class:`pharmpy.model`. Effects that currently have templates are:

- Additive (*add*)

- Proportional (*prop*)

- Exponential (*exp*)

- Logit (*logit*)

For all except exponential the operation input is not needed. Otherwise user specified

input is supported. Initial estimates for new etas are 0.09.

This documentation was automatically generated from Pharmpy (0.25.1).}
\arguments{

\item{
model
}{
Model. Pharmpy model to add new IIVs to.
}
\item{
list_of_parameters
}{
character, vector. Name/names of parameter to add new IIVs to.
}
\item{
expression
}{
character, vector. Effect/effects on eta. Either abbreviated (see above) or custom.
}
\item{
operation
}{
character, vector, optional. Whether the new IIV should be added or multiplied (default).
}
\item{
eta_names
}{
character, vector, optional. Custom name/names of new eta
}
}
