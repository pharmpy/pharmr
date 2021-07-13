\name{john_draper}
\alias{john_draper}
\title{john_draper}
\description{
Applies a John Draper transformation [1]_ to specified etas from a.

\bold{Usage}

\code{john_draper(model, list_of_etas=NULL)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.john_draper}{API reference} (for correct rendering of equations, tables etc.).
Applies a John Draper transformation [1]_ to specified etas from a

:class:`pharmpy.model`. Initial estimate for lambda is 0.1 with bounds (-3, 3).

.. [1] John, J., Draper, N. (1980). An Alternative Family of Transformations.

Journal of the Royal Statistical Society. Series C (Applied Statistics),

29(2), 190-197. doi:10.2307/2986305

This documentation was automatically generated from Pharmpy (0.26.0).}
\arguments{

\item{
model
}{
Model. Pharmpy model to apply John Draper transformation to.
}
\item{
list_of_etas
}{
character, vector. Name/names of etas to transform. If NULL, all etas will be transformed (default).
}
}
