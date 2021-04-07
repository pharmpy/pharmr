\name{power_on_ruv}
\alias{power_on_ruv}
\title{power_on_ruv}
\description{
Applies a power effect to provided epsilons.

\bold{Usage}

\code{power_on_ruv(model, list_of_eps=NULL)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.power_on_ruv}{API reference} (for correct rendering of equations, tables etc.).
Applies a power effect to provided epsilons. Initial estimates for new thetas are 1 if the error

model is proportional, otherwise they are 0.1.

This documentation was automatically generated from Pharmpy (0.22.0).}
\arguments{

\item{
model
}{
Model. Pharmpy model to create block effect on.
}
\item{
list_of_eps
}{
character, vector. Name/names of epsilons to apply power effect. If None, all epsilons will be used. None is default.
}
}
