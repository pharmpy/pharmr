\name{iiv_on_ruv}
\alias{iiv_on_ruv}
\title{iiv_on_ruv}
\description{
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html?highlight=add_cov#pharmpy.modeling.iiv_on_ruv}{Link to Python API reference} (e.g. for correct rendering of equations)

Multiplies epsilons with exponential (new) etas. Initial estimates for new etas are 0.09.
}
\usage{
iiv_on_ruv(model, list_of_eps=None, same_eta=True, eta_names=None)
}
\arguments{

\item{
model
}{
Model, Pharmpy model to apply IIV on epsilons.
}
\item{
list_of_eps
}{
list, List of epsilons to multiply with exponential etas. If None, all epsilons will
}
\item{
same_eta
}{
bool, Boolean of whether all RUVs from input should use the same new ETA or if one ETA
}
}
\value{

}
