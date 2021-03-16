\name{fix_parameters_to}
\alias{fix_parameters_to}
\title{fix_parameters_to}
\description{
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html?highlight=add_cov#pharmpy.modeling.fix_parameters_to}{Link to Python API reference} (e.g. for correct rendering of equations)

Fix parameters to

Fix all listed parameters to specified value/values
}
\usage{
fix_parameters_to(model, parameter_names, values)
}
\arguments{

\item{
model
}{
Model, parameter_names : list or str
}
\item{
parameter_names
}{
list or str, one parameter name or a list of parameter names
}
\item{
values
}{
list or int, one value or a list of values (must be equal to number of parameter_names)
}
}
\value{
model : Model
}
