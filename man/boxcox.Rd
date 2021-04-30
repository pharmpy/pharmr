\name{boxcox}
\alias{boxcox}
\title{boxcox}
\description{
Applies a boxcox transformation to specified etas from a :class:`pharmpy.model`.

\bold{Usage}

\code{boxcox(model, list_of_etas=NULL)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.boxcox}{API reference} (for correct rendering of equations, tables etc.).
Applies a boxcox transformation to specified etas from a :class:`pharmpy.model`. Initial

estimate for lambda is 0.1 with bounds (-3, 3).

This documentation was automatically generated from Pharmpy (0.23.3).}
\arguments{

\item{
model
}{
Model. Pharmpy model to apply boxcox transformation to.
}
\item{
list_of_etas
}{
character, vector. Name/names of etas to transform. If NULL, all etas will be transformed (default).
}
}
