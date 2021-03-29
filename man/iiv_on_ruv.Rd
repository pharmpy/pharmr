\name{iiv_on_ruv}
\alias{iiv_on_ruv}
\title{iiv_on_ruv}
\description{
Multiplies epsilons with exponential (new) etas.

\bold{Usage}

\code{iiv_on_ruv(model, list_of_eps=None, same_eta=True, eta_names=None)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html}{API reference} (for correct rendering of equations, tables etc.).
Multiplies epsilons with exponential (new) etas. Initial estimates for new etas are 0.09.

This documentation was automatically generated from Pharmpy (0.22.0).}
\arguments{

\item{
model
}{
Model, Pharmpy model to apply IIV on epsilons.
}
\item{
list_of_eps
}{
str, list, Name/names of epsilons to multiply with exponential etas. If None, all epsilons will
}
\item{
same_eta
}{
bool, Boolean of whether all RUVs from input should use the same new ETA or if one ETA
}
\item{
eta_names
}{
str, list, Custom names of new etas. Must be equal to the number epsilons or 1 if same eta.
}
}
