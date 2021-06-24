\name{iiv_on_ruv}
\alias{iiv_on_ruv}
\title{iiv_on_ruv}
\description{
Multiplies epsilons with exponential (new) etas.

\bold{Usage}

\code{iiv_on_ruv(model, list_of_eps=NULL, same_eta=TRUE, eta_names=NULL)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.iiv_on_ruv}{API reference} (for correct rendering of equations, tables etc.).
Multiplies epsilons with exponential (new) etas. Initial estimates for new etas are 0.09.

This documentation was automatically generated from Pharmpy (0.25.0).}
\arguments{

\item{
model
}{
Model. Pharmpy model to apply IIV on epsilons.
}
\item{
list_of_eps
}{
character, vector. Name/names of epsilons to multiply with exponential etas. If NULL, all epsilons will be chosen. NULL is default.
}
\item{
same_eta
}{
bool. Boolean of whether all RUVs from input should use the same new ETA or if one ETA should be created for each RUV. TRUE is default.
}
\item{
eta_names
}{
character, vector. Custom names of new etas. Must be equal to the number epsilons or 1 if same eta.
}
}
