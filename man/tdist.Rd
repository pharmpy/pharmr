\name{tdist}
\alias{tdist}
\title{tdist}
\description{
Applies a t-distribution transformation to specified etas from a :class:`pharmpy.model`.

\bold{Usage}

\code{tdist(model, list_of_etas=NULL)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.tdist}{API reference} (for correct rendering of equations, tables etc.).
Applies a t-distribution transformation to specified etas from a :class:`pharmpy.model`. Initial

estimate for degrees of freedom is 80 with bounds (3, 100).

This documentation was automatically generated from Pharmpy (0.25.1).}
\arguments{

\item{
model
}{
Model. Pharmpy model to apply t distribution transformation to.
}
\item{
list_of_etas
}{
character, vector. Name/names of etas to transform. If NULL, all etas will be transformed (default).
}
}
