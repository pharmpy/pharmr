\name{tdist}
\alias{tdist}
\title{tdist}
\description{
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html?highlight=add_cov#pharmpy.modeling.tdist}{Link to Python API reference} (e.g. for correct rendering of equations)

Applies a t-distribution transformation to specified etas from a :class:`pharmpy.model`. Initial

estimate for degrees of freedom is 80 with bounds (3, 100).
}
\usage{
tdist(model, list_of_etas=None)
}
\arguments{

\item{
model
}{
Model, Pharmpy model to apply t distribution transformation to.
}
\item{
list_of_etas
}{
list, List of etas to transform. If None, all etas will be transformed (default).
}
}
\value{

}
