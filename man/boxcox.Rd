\name{boxcox}
\alias{boxcox}
\title{boxcox}
\description{
Applies a boxcox transformation to specified etas from a :class:`pharmpy.model`.

\bold{Usage}

\code{boxcox(model, list_of_etas=None)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html}{API reference} (for correct rendering of equations, tables etc.).
Applies a boxcox transformation to specified etas from a :class:`pharmpy.model`. Initial

estimate for lambda is 0.1 with bounds (-3, 3).
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
str, list, Name/names of etas to transform. If None, all etas will be transformed (default).
}
}