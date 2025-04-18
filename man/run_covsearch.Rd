% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_wrapper.R
\name{run_covsearch}
\alias{run_covsearch}
\title{run_covsearch}
\usage{
run_covsearch(
  model,
  results,
  search_space,
  p_forward = 0.01,
  p_backward = 0.001,
  max_steps = -1,
  algorithm = "scm-forward-then-backward",
  max_eval = FALSE,
  adaptive_scope_reduction = FALSE,
  strictness = "minimization_successful or (rounding_errors and sigdigs>=0.1)",
  naming_index_offset = 0,
  nsamples = 10,
  .samba_max_covariates = 3,
  .samba_selection_criterion = "bic",
  .samba_linreg_method = "ols",
  .samba_stepwise_lcs = NULL,
  ...
)
}
\arguments{
\item{model}{(Model) Pharmpy model}

\item{results}{(ModelfitResults) Results of model}

\item{search_space}{(str or ModelFeatures) MFL of covariate effects to try}

\item{p_forward}{(numeric) The p-value to use in the likelihood ratio test for forward steps}

\item{p_backward}{(numeric) The p-value to use in the likelihood ratio test for backward steps}

\item{max_steps}{(numeric) The maximum number of search steps to make}

\item{algorithm}{(str) The search algorithm to use. Currently, 'scm-forward' and
'scm-forward-then-backward' are supported.}

\item{max_eval}{(logical) Limit the number of function evaluations to 3.1 times that of the
base model. Default is FALSE.}

\item{adaptive_scope_reduction}{(logical) Stash all non-significant parameter-covariate effects to be tested
after all significant effects have been tested. Once all these have been
tested, try adding the stashed effects once more with a regular forward approach.
Default is FALSE}

\item{strictness}{(str) Strictness criteria}

\item{naming_index_offset}{(numeric (optional)) index offset for naming of runs. Default is 0.}

\item{nsamples}{(numeric) Number of samples from individual parameter conditional distribution for linear covariate model selection.
Default is 10, i.e. generating 10 samples per subject}

\item{.samba_max_covariates}{(numeric (optional)) Maximum number of covariate inclusion allowed in linear covariate screening for each parameter.}

\item{.samba_selection_criterion}{(str) Method used to fit linear covariate models. Currently, Ordinary Least Squares (ols),
Weighted Least Squares (wls), and Linear Mixed-Effects (lme) are supported.}

\item{.samba_linreg_method}{(str) Method used for linear and nonlinear model selection in SAMBA methods. Currently, BIC and LRT are
supported.}

\item{.samba_stepwise_lcs}{(logical (optional)) Use stepwise linear covariate screening or not. By default, SAMBA methods use stepwise LCS whereas SCM-LCS uses
non-stepwise LCS}

\item{...}{Arguments to pass to tool}
}
\value{
(COVSearchResults) COVsearch tool result object
}
\description{
Run COVsearch tool. For more details, see :ref:\code{covsearch}.
}
\examples{
\dontrun{
model <- load_example_model("pheno")
results <- load_example_modelfit_results("pheno")
search_space <- 'COVARIATE(c(CL, V), c(AGE, WT), EXP)'
res <- run_covsearch(model=model, results=results, search_space=search_space)
}

}
