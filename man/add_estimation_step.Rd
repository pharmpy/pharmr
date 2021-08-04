\name{add_estimation_step}
\alias{add_estimation_step}
\title{add_estimation_step}
\description{
Add estimation step.

\bold{Usage}

\code{add_estimation_step(model, method, interaction=TRUE, options={}, est_idx=NULL)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.add_estimation_step}{API reference} (for correct rendering of equations, tables etc.).
Add estimation step

Adds estimation step for a model in a given index. Methods currently supported are:

FO, FOCE, ITS, LAPLACE, IMPMAP, IMP, SAEM

This documentation was automatically generated from Pharmpy (0.26.1).}
\arguments{

\item{
model
}{
Model. Pharmpy model
}
\item{
method
}{
character. estimation method to change to
}
\item{
interaction
}{
bool. whether to use interaction or not, default is true
}
\item{
options
}{
list. any additional options. Note that this removes old options
}
\item{
est_idx
}{
numeric. index of estimation step, default is NULL (adds step last)
}
}
\value{
model : Model
}
