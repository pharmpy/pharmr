\name{add_iov}
\alias{add_iov}
\title{add_iov}
\description{
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html?highlight=add_cov#pharmpy.modeling.add_iov}{Link to Python API reference} (e.g. for correct rendering of equations)

Adds IOVs to :class:`pharmpy.model`. Initial estimate of new IOVs are 10% of the IIV eta

it is based on.
}
\usage{
add_iov(model, occ, list_of_parameters=None, eta_names=None)
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
}
\value{

}
