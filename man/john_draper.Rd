\name{john_draper}
\alias{john_draper}
\title{john_draper}
\description{
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html?highlight=add_cov#pharmpy.modeling.john_draper}{Link to Python API reference} (e.g. for correct rendering of equations)

Applies a John Draper transformation [1]_ to specified etas from a

:class:`pharmpy.model`. Initial estimate for lambda is 0.1 with bounds (-3, 3).

.. [1] John, J., Draper, N. (1980). An Alternative Family of Transformations.

Journal of the Royal Statistical Society. Series C (Applied Statistics),

29(2), 190-197. doi:10.2307/2986305
}
\usage{
john_draper(model, list_of_etas=None)
}
\arguments{

\item{
model
}{
Model, Pharmpy model to apply John Draper transformation to.
}
\item{
list_of_etas
}{
list, List of etas to transform. If None, all etas will be transformed (default).
}
}
\value{

}
