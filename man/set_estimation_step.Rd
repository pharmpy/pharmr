\name{set_estimation_step}
\alias{set_estimation_step}
\title{set_estimation_step}
\description{
Set estimation step.

\bold{Usage}

\code{set_estimation_step(model, method, interaction=TRUE, options={}, est_idx=0)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.set_estimation_step}{API reference} (for correct rendering of equations, tables etc.).
Set estimation step

Sets estimation step for a model. Methods currently supported are:

FO, FOCE, ITS, LAPLACE, IMPMAP, IMP, SAEM

This documentation was automatically generated from Pharmpy (0.26.0).}
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
numeric. index of estimation step, default is 0 (first estimation step)
}
}
\value{
model : Model
}
