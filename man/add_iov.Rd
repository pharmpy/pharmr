\name{add_iov}
\alias{add_iov}
\title{add_iov}
\description{
Adds IOVs to :class:`pharmpy.model`.

\bold{Usage}

\code{add_iov(model, occ, list_of_parameters=None, eta_names=None)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html}{API reference} (for correct rendering of equations, tables etc.).
Adds IOVs to :class:`pharmpy.model`. Initial estimate of new IOVs are 10% of the IIV eta

it is based on.
}
\arguments{

\item{
model
}{
Model, Pharmpy model to add new IOVs to.
}
\item{
occ
}{
str, Name of occasion column.
}
\item{
list_of_parameters
}{
str, list, List of names of parameters and random variables. Accepts random variable names, parameter
}
\item{
eta_names
}{
str, list, Custom names of new etas. Must be equal to the number of input etas times the number of
}
}
