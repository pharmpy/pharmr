\name{boxcox}
\alias{boxcox}
\title{boxcox}
\description{
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html?highlight=add_cov#pharmpy.modeling.boxcox}{Link to Python API reference} (e.g. for correct rendering of equations)

Applies a boxcox transformation to specified etas from a :class:`pharmpy.model`. Initial

estimate for lambda is 0.1 with bounds (-3, 3).
}
\usage{
boxcox(model, list_of_etas=None)
}
\arguments{

\item{
model
}{
Model, Pharmpy model to apply boxcox transformation to.
}
\item{
list_of_etas
}{
list, List of etas to transform. If None, all etas will be transformed (default).
}
}
\value{

}
