\name{remove_iiv}
\alias{remove_iiv}
\title{remove_iiv}
\description{
Removes all IIV omegas given a list with eta names and/or parameter names..

\bold{Usage}

\code{remove_iiv(model, list_to_remove=NULL)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.remove_iiv}{API reference} (for correct rendering of equations, tables etc.).
Removes all IIV omegas given a list with eta names and/or parameter names.

This documentation was automatically generated from Pharmpy (0.25.1).}
\arguments{

\item{
model
}{
Model. Pharmpy model to create block effect on.
}
\item{
list_to_remove
}{
character, vector. Name/names of etas and/or name/names of individual parameters to remove. If NULL, all etas that are IIVs will be removed. NULL is default.
}
}
