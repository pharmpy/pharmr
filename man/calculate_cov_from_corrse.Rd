% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{calculate_cov_from_corrse}
\alias{calculate_cov_from_corrse}
\title{calculate_cov_from_corrse}
\usage{
calculate_cov_from_corrse(corr, se)
}
\arguments{
\item{corr}{(data.frame) Correlation matrix}

\item{se}{(array) Standard errors}
}
\value{
(data.frame) Covariance matrix
}
\description{
Calculate covariance matrix from a correlation matrix and standard errors
}
\examples{
\dontrun{
results <- load_example_modelfit_results("pheno")
corr <- results$correlation_matrix
se <- results$standard_errors
corr
calculate_cov_from_corrse(corr, se)
}
}
\seealso{
calculate_se_from_cov : Standard errors from covariance matrix

calculate_se_from_prec : Standard errors from precision matrix

calculate_corr_from_cov : Correlation matrix from covariance matrix

calculate_cov_from_prec : Covariance matrix from precision matrix

calculate_prec_from_cov : Precision matrix from covariance matrix

calculate_prec_from_corrse : Precision matrix from correlation matrix and standard errors

calculate_corr_from_prec : Correlation matrix from precision matrix
}
