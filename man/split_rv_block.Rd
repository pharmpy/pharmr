\name{split_rv_block}
\alias{split_rv_block}
\title{split_rv_block}
\description{
Splits a block structure given a list of etas to separate..

\bold{Usage}

\code{split_rv_block(model, list_of_rvs=NULL)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.split_rv_block}{API reference} (for correct rendering of equations, tables etc.).
Splits a block structure given a list of etas to separate.

This documentation was automatically generated from Pharmpy (0.24.2).}
\arguments{

\item{
model
}{
Model. Pharmpy model to create block effect on.
}
\item{
list_of_rvs
}{
character, vector. Name/names of etas to split from block structure. If NULL, all etas that are IIVs and non-fixed will become single. NULL is default.
}
}
