\name{evaluate_expression}
\alias{evaluate_expression}
\title{evaluate_expression}
\description{
Evaluate expression using model.

\bold{Usage}

\code{evaluate_expression(model, expression)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.evaluate_expression}{API reference} (for correct rendering of equations, tables etc.).
Evaluate expression using model

Calculate the value of expression for each data record.

The expression can contain dataset columns, variables in model and

population parameters. If the model has parameter estimates these

will be used. Initial estimates will be used for non-estimated parameters.

This documentation was automatically generated from Pharmpy (0.26.0).}
\arguments{

\item{
expression
}{
character or sympy expression. Expression to evaluate
}
}
\value{
pd.Series
A series of one evaluated value for each data record
}
