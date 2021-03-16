\name{create_rv_block}
\alias{create_rv_block}
\title{create_rv_block}
\description{
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html?highlight=add_cov#pharmpy.modeling.create_rv_block}{Link to Python API reference} (e.g. for correct rendering of equations)

Creates a full or partial block structure of etas. The etas must be IIVs and cannot

be fixed. Initial estimates for covariance between the etas is dependent on whether

the model has results from a previous results. In that case, the correlation will

be calculated from individual estimates, otherwise correlation will be set to 10%.
}
\usage{
create_rv_block(model, list_of_rvs=None)
}
\arguments{

\item{
model
}{
Model, Pharmpy model to create block effect on.
}
\item{
list_of_rvs
}{
list, List of etas to create a block structure from. If None, all etas that are IIVs and
}
}
\value{

}
