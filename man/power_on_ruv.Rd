\name{power_on_ruv}
\alias{power_on_ruv}
\title{power_on_ruv}
\description{
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html?highlight=add_cov#pharmpy.modeling.power_on_ruv}{Link to Python API reference} (e.g. for correct rendering of equations)

Applies a power effect to provided epsilons. Initial estimates for new thetas are 1 if the error

model is proportional, otherwise they are 0.1.
}
\usage{
power_on_ruv(model, list_of_eps=None)
}
\arguments{

\item{
model
}{
Model, Pharmpy model to create block effect on.
}
\item{
list_of_eps
}{
list, List of epsilons to apply power effect. If None, all epsilons will be used.
}
}
\value{

}
