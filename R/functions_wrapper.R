#' @title
#' add_allometry
#' 
#' @description
#' Add allometric scaling of parameters
#' 
#' Add an allometric function to each listed parameter. The function will be
#' P=P*(X/Z)**T where P is the parameter, X the allometric_variable, Z the reference_value
#' and T is a theta. Default is to automatically use clearance and volume parameters.
#' 
#' @param model (Model) Pharmpy model
#' @param allometric_variable (str or sympy.Expr) Value to use for allometry (X above)
#' @param reference_value (str, integer, numeric or sympy.Expr) Reference value (Z above)
#' @param parameters (vector) Parameters to use or NULL (default) for all available CL, Q and V parameters
#' @param initials (vector) Initial estimates for the exponents. Default is to use 0.75 for CL and Qs and 1 for Vs
#' @param lower_bounds (vector) Lower bounds for the exponents. Default is 0 for all parameters
#' @param upper_bounds (vector) Upper bounds for the exponents. Default is 2 for all parameters
#' @param fixed (logical) Whether the exponents should be fixed
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' add_allometry(model, allometric_variable='WGT')
#' model$statements$before_odes
#' }
#' 
#' @export
add_allometry <- function(model, allometric_variable='WT', reference_value=70, parameters=NULL, initials=NULL, lower_bounds=NULL, upper_bounds=NULL, fixed=TRUE) {
	func_out <- pharmpy$modeling$add_allometry(model, allometric_variable=allometric_variable, reference_value=reference_value, parameters=parameters, initials=initials, lower_bounds=lower_bounds, upper_bounds=upper_bounds, fixed=fixed)
	return(py_to_r(func_out))
}

#' @title
#' add_covariance_step
#' 
#' @description
#' Adds covariance step to the final estimation step
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_estimation_step(model, 'FOCE', cov=FALSE)
#' add_covariance_step(model)
#' ests <- model$estimation_steps
#' ests[1]
#' }
#' @seealso
#' add_estimation_step
#' 
#' set_estimation_step
#' 
#' remove_estimation_step
#' 
#' append_estimation_step_options
#' 
#' remove_covariance_step
#' 
#' set_evaluation_step
#' 
#' 
#' @export
add_covariance_step <- function(model) {
	func_out <- pharmpy$modeling$add_covariance_step(model)
	return(py_to_r(func_out))
}

#' @title
#' add_covariate_effect
#' 
#' @description
#' Adds covariate effect to :class:`pharmpy.model`.
#' 
#' The following effects have templates:
#' 
#' * Linear function for continuous covariates (*lin*)
#' * Function:
#' 
#' math::
#' 
#' {coveff} = 1 + {theta} * ({cov} - {median})
#' 
#' * Init:  0.001
#' * Upper:
#' * If median of covariate equals minimum: :math:`100,000`
#' * Otherwise: :math:`frac{1}{{median} - {min}}`
#' * Lower:
#' * If median of covariate equals maximum: :math:`-100,000`
#' * Otherwise: :math:`frac{1}{{median} - {max}}`
#' * Linear function for categorical covariates (*cat*)
#' * Function:
#' 
#' * If covariate is most common category:
#' 
#' math::
#' 
#' {coveff} = 1
#' 
#' * For each additional category:
#' 
#' math::
#' 
#' {coveff} = 1 + {theta}
#' 
#' * Init: :math:`0.001`
#' * Upper: :math:`100,000`
#' * Lower: :math:`-100,000`
#' * Piecewise linear function/"hockey-stick", continuous covariates only (*piece_lin*)
#' * Function:
#' * If cov <= median:
#' 
#' math::
#' 
#' {coveff} = 1 + {theta1} * ({cov} - {median})
#' 
#' * If cov > median:
#' 
#' math::
#' 
#' {coveff} = 1 + {theta2} * ({cov} - {median})
#' 
#' 
#' * Init: :math:`0.001`
#' * Upper:
#' * For first state: :math:`frac{1}{{median} - {min}}`
#' * Otherwise: :math:`100,000`
#' * Lower:
#' * For first state: :math:`-100,000`
#' * Otherwise: :math:`frac{1}{{median} - {max}}`
#' * Exponential function, continuous covariates only (*exp*)
#' * Function:
#' 
#' math::
#' 
#' {coveff} = exp({theta} * ({cov} - {median}))
#' 
#' * Init:
#' * If lower > 0.001 or upper < 0.001: :math:`frac{{upper} - {lower}}{2}`
#' * If estimated init is 0: :math:`frac{{upper}}{2}`
#' * Otherwise: :math:`0.001`
#' * Upper:
#' * If min - median = 0 or max - median = 0: :math:`100`
#' * Otherwise:
#' 
#' math::
#' 
#' min(frac{log(0.01)}{{min} - {median}},
#' frac{log(100)}{{max} - {median}})
#' * Lower:
#' * If min - median = 0 or max - median = 0: :math:`0.01`
#' * Otherwise:
#' 
#' math::
#' 
#' max(frac{log(0.01)}{{max} - {median}},
#' frac{log(100)}{{min} - {median}})
#' 
#' * Power function, continuous covariates only (*pow*)
#' * Function:
#' 
#' math::
#' 
#' {coveff} = (frac{{cov}}{{median}})^{theta}
#' 
#' * Init: :math:`0.001`
#' * Upper: :math:`100,000`
#' * Lower: :math:`-100`
#' 
#' 
#' @param model (Model) Pharmpy model to add covariate effect to.
#' @param parameter (str) Name of parameter to add covariate effect to.
#' @param covariate (str) Name of covariate.
#' @param effect (str) Type of covariate effect. May be abbreviated covariate effect (see above) or custom.
#' @param operation (str, optional) Whether the covariate effect should be added or multiplied (default).
#' @param allow_nested (logical, optional) Whether to allow adding a covariate effect when one already exists for
#'  the input parameter-covariate pair.
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' add_covariate_effect(model, "CL", "APGR", "exp")
#' model$statements$before_odes$full_expression("CL")
#' }
#' 
#' @export
add_covariate_effect <- function(model, parameter, covariate, effect, operation='*', allow_nested=FALSE) {
	func_out <- pharmpy$modeling$add_covariate_effect(model, parameter, covariate, effect, operation=operation, allow_nested=allow_nested)
	return(py_to_r(func_out))
}

#' @title
#' add_estimation_step
#' 
#' @description
#' Add estimation step
#' 
#' Adds estimation step for a model in a given index. Methods currently supported are:
#' FO, FOCE, ITS, LAPLACE, IMPMAP, IMP, SAEM
#' 
#' @param model (Model) Pharmpy model
#' @param method (str) estimation method to change to
#' @param idx (integer) index of estimation step (starting from 0), default is NULL (adds step at the end)
#' @param ... Arguments to pass to EstimationStep (such as interaction, evaluation)
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' opts <- list('NITER'=1000, 'ISAMPLE'=100)
#' add_estimation_step(model, "IMP", tool_options=opts)
#' ests <- model$estimation_steps
#' length(ests)
#' ests[2]
#' }
#' @seealso
#' set_estimation_step
#' 
#' remove_estimation_step
#' 
#' append_estimation_step_options
#' 
#' add_covariance_step
#' 
#' remove_covariance_step
#' 
#' set_evaluation_step
#' 
#' 
#' @export
add_estimation_step <- function(model, method, idx=NULL, ...) {
	func_out <- pharmpy$modeling$add_estimation_step(model, method, idx=idx, ...)
	return(py_to_r(func_out))
}

#' @title
#' add_iiv
#' 
#' @description
#' Adds IIVs to :class:`pharmpy.model`.
#' 
#' Effects that currently have templates are:
#' 
#' * Additive (*add*)
#' * Proportional (*prop*)
#' * Exponential (*exp*)
#' * Logit (*log*)
#' 
#' For all except exponential the operation input is not needed. Otherwise user specified
#' input is supported. Initial estimates for new etas are 0.09.
#' 
#' @param model (Model) Pharmpy model to add new IIVs to.
#' @param list_of_parameters (str, vector) Name/names of parameter to add new IIVs to.
#' @param expression (str, vector) Effect/effects on eta. Either abbreviated (see above) or custom.
#' @param operation (str, vector, optional) Whether the new IIV should be added or multiplied (default).
#' @param initial_estimate (numeric) Value of initial estimate of parameter. Default is 0.09
#' @param eta_names (str, vector, optional) Custom name/names of new eta
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' remove_iiv(model, "CL")
#' add_iiv(model, "CL", "add")
#' model$statements$find_assignment("CL")
#' }
#' @seealso
#' add_pk_iiv
#' 
#' add_iov
#' 
#' remove_iiv
#' 
#' remove_iov
#' 
#' 
#' @export
add_iiv <- function(model, list_of_parameters, expression, operation='*', initial_estimate=0.09, eta_names=NULL) {
	func_out <- pharmpy$modeling$add_iiv(model, list_of_parameters, expression, operation=operation, initial_estimate=initial_estimate, eta_names=eta_names)
	return(py_to_r(func_out))
}

#' @title
#' add_individual_parameter
#' 
#' @description
#' Add an individual or pk parameter to a model
#' 
#' @param model (Model) Pharmpy model
#' @param name (str) Name of individual/pk parameter
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' add_individual_parameter(model, "KA")
#' model$statements$find_assignment("KA")
#' }
#' 
#' @export
add_individual_parameter <- function(model, name) {
	func_out <- pharmpy$modeling$add_individual_parameter(model, name)
	return(py_to_r(func_out))
}

#' @title
#' add_iov
#' 
#' @description
#' Adds IOVs to :class:`pharmpy.model`.
#' 
#' Initial estimate of new IOVs are 10% of the IIV eta it is based on.
#' 
#' @param model (Model) Pharmpy model to add new IOVs to.
#' @param occ (str) Name of occasion column.
#' @param list_of_parameters (str, vector) List of names of parameters and random variables. Accepts random variable names, parameter
#'  names, or a mix of both.
#' @param eta_names (str, vector) Custom names of new etas. Must be equal to the number of input etas times the number of
#'  categories for occasion.
#' @param distribution (str) The distribution that should be used for the new etas. Options are
#'  'disjoint' for disjoint normal distributions, 'joint' for joint normal
#'  distribution, 'explicit' for an explicit mix of joint and disjoint
#'  distributions, and 'same-as-iiv' for copying the distribution of IIV etas.
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' add_iov(model, "TIME", "CL")
#' model$statements$find_assignment("CL")
#' }
#' @seealso
#' add_iiv
#' 
#' add_pk_iiv
#' 
#' remove_iiv
#' 
#' remove_iov
#' 
#' 
#' @export
add_iov <- function(model, occ, list_of_parameters=NULL, eta_names=NULL, distribution='disjoint') {
	func_out <- pharmpy$modeling$add_iov(model, occ, list_of_parameters=list_of_parameters, eta_names=eta_names, distribution=distribution)
	return(py_to_r(func_out))
}

#' @title
#' add_lag_time
#' 
#' @description
#' Add lag time to the dose compartment of model.
#' 
#' Initial estimate for lag time is set the
#' previous lag time if available, otherwise it is set to the time of first observation/2.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' add_lag_time(model)
#' }
#' @seealso
#' set_transit_compartments
#' 
#' remove_lag_time
#' 
#' 
#' @export
add_lag_time <- function(model) {
	func_out <- pharmpy$modeling$add_lag_time(model)
	return(py_to_r(func_out))
}

#' @title
#' add_peripheral_compartment
#' 
#' @description
#' Add a peripheral distribution compartment to model
#' 
#' The rate of flow from the central to the peripheral compartment
#' will be parameterized as QPn / VC where VC is the volume of the central compartment.
#' The rate of flow from the peripheral to the central compartment
#' will be parameterized as QPn / VPn where VPn is the volumne of the added peripheral
#' compartment.
#' 
#' Initial estimates:
#' 
#' ==  ===================================================
#' n
#' ==  ===================================================
#' 1   :math:`{CL} = {CL'}`, :math:`{VC} = {VC'}`,
#' :math:`{QP1} = {CL'}` and :math:`{VP1} = {VC'} * 0.05`
#' 2   :math:`{QP1} = {QP1' * 0.1}`, :math:`{VP1} = {VP1'}`,
#' :math:`{QP2} = {QP1' * 0.9}` and :math:`{VP2} = {VP1'}`
#' ==  ===================================================
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' add_peripheral_compartment(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_peripheral_compartment
#' 
#' remove_peripheral_compartment
#' 
#' 
#' @export
add_peripheral_compartment <- function(model) {
	func_out <- pharmpy$modeling$add_peripheral_compartment(model)
	return(py_to_r(func_out))
}

#' @title
#' add_pk_iiv
#' 
#' @description
#' Adds IIVs to all PK parameters in :class:`pharmpy.model`.
#' 
#' Will add exponential IIVs to all parameters that are included in the ODE.
#' 
#' @param model (Model) Pharmpy model to add new IIVs to.
#' @param initial_estimate (numeric) Value of initial estimate of parameter. Default is 0.09
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_first_order_absorption(model)
#' model$statements$find_assignment("MAT")
#' add_pk_iiv(model)
#' model$statements$find_assignment("MAT")
#' }
#' @seealso
#' add_iiv
#' 
#' add_iov
#' 
#' remove_iiv
#' 
#' remove_iov
#' 
#' 
#' @export
add_pk_iiv <- function(model, initial_estimate=0.09) {
	func_out <- pharmpy$modeling$add_pk_iiv(model, initial_estimate=initial_estimate)
	return(py_to_r(func_out))
}

#' @title
#' add_population_parameter
#' 
#' @description
#' Add a new population parameter to the model
#' 
#' @param model (Model) Pharmpy model
#' @param name (str) Name of the new parameter
#' @param init (numeric) Initial estimate of the new parameter
#' @param lower (numeric) Lower bound of the new parameter
#' @param upper (numeric) Upper bound of the new parameter
#' @param fix (logical) Should the new parameter be fixed?
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' add_population_parameter(model, 'POP_KA', 2)
#' model$parameters
#' }
#' 
#' @export
add_population_parameter <- function(model, name, init, lower=NULL, upper=NULL, fix=FALSE) {
	func_out <- pharmpy$modeling$add_population_parameter(model, name, init, lower=lower, upper=upper, fix=fix)
	return(py_to_r(func_out))
}

#' @title
#' add_time_after_dose
#' 
#' @description
#' Calculate and add a TAD column to the dataset"
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' add_time_after_dose(model)
#' }
#' 
#' @export
add_time_after_dose <- function(model) {
	func_out <- pharmpy$modeling$add_time_after_dose(model)
	return(py_to_r(func_out))
}

#' @title
#' append_estimation_step_options
#' 
#' @description
#' Append estimation step options
#' 
#' Appends options to an existing estimation step.
#' 
#' @param model (Model) Pharmpy model
#' @param tool_options (list) any additional tool specific options
#' @param idx (integer) index of estimation step (starting from 0)
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' opts <- list('NITER'=1000, 'ISAMPLE'=100)
#' append_estimation_step_options(model, tool_options=opts, idx=0)
#' est <- model$estimation_steps[1]
#' length(est$tool_options)
#' }
#' @seealso
#' add_estimation_step
#' 
#' set_estimation_step
#' 
#' remove_estimation_step
#' 
#' add_covariance_step
#' 
#' remove_covariance_step
#' 
#' set_evaluation_step
#' 
#' 
#' @export
append_estimation_step_options <- function(model, tool_options, idx) {
	func_out <- pharmpy$modeling$append_estimation_step_options(model, tool_options, idx)
	return(py_to_r(func_out))
}

#' @title
#' bump_model_number
#' 
#' @description
#' If the model name ends in a number increase it
#' 
#' If path is set increase the number until no file exists
#' with the same name in path.
#' If model name does not end in a number do nothing.
#' 
#' @param model (Model) Pharmpy model object
#' @param path (Path in which to find next unique number) Default is to not look for files.
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$name <- "run2"
#' bump_model_number(model)
#' model$name
#' }
#' 
#' @export
bump_model_number <- function(model, path=NULL) {
	func_out <- pharmpy$modeling$bump_model_number(model, path=path)
	return(py_to_r(func_out))
}

#' @title
#' calculate_aic
#' 
#' @description
#' Calculate AIC
#' 
#' AIC = -2LL + 2*n_estimated_parameters
#' 
#' @param model (Model) Pharmpy model object
#' @param likelihood (numeric) -2LL
#'  
#' @return (numeric) AIC of model fit
#' 
#' 
#' @export
calculate_aic <- function(model, likelihood) {
	func_out <- pharmpy$modeling$calculate_aic(model, likelihood)
	return(py_to_r(func_out))
}

#' @title
#' calculate_bic
#' 
#' @description
#' Calculate BIC
#' 
#' Different variations of the BIC can be calculated:
#' 
#' * | mixed (default)
#' | BIC = -2LL + n_random_parameters * log(n_individuals) +
#' |       n_fixed_parameters * log(n_observations)
#' * | fixed
#' | BIC = -2LL + n_estimated_parameters * log(n_observations)
#' * | random
#' | BIC = -2LL + n_estimated_parameters * log(n_individals)
#' * | iiv
#' | BIC = -2LL + n_estimated_iiv_omega_parameters * log(n_individals)
#' 
#' @param model (Model) Pharmpy model object
#' @param likelihood (numeric) -2LL to use
#' @param type (str) Type of BIC to calculate. Default is the mixed effects.
#'  
#' @return (numeric) BIC of model fit
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' ofv <- model$modelfit_results$ofv
#' calculate_bic(model, ofv)
#' calculate_bic(model, ofv, type='fixed')
#' calculate_bic(model, ofv, type='random')
#' calculate_bic(model, ofv, type='iiv')
#' }
#' 
#' @export
calculate_bic <- function(model, likelihood, type=NULL) {
	func_out <- pharmpy$modeling$calculate_bic(model, likelihood, type=type)
	return(py_to_r(func_out))
}

#' @title
#' calculate_corr_from_cov
#' 
#' @description
#' Calculate correlation matrix from a covariance matrix
#' 
#' @param cov (data.frame) Covariance matrix
#'  
#' @return (data.frame) Correlation matrix
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' cov <- model$modelfit_results$covariance_matrix
#' cov
#' calculate_corr_from_cov(cov)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_se_from_inf : Standard errors from information matrix
#' 
#' calculate_cov_from_inf : Covariance matrix from information matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_inf_from_cov : Information matrix from covariance matrix
#' 
#' calculate_inf_from_corrse : Information matrix from correlation matrix and standard errors
#' 
#' calculate_corr_from_inf : Correlation matrix from information matrix
#' 
#' 
#' @export
calculate_corr_from_cov <- function(cov) {
	func_out <- pharmpy$modeling$calculate_corr_from_cov(cov)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_corr_from_inf
#' 
#' @description
#' Calculate correlation matrix from an information matrix
#' 
#' @param information_matrix (data.frame) Information matrix
#'  
#' @return (data.frame) Correlation matrix
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' inf <- model$modelfit_results$information_matrix
#' inf
#' calculate_corr_from_inf(inf)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_se_from_inf : Standard errors from information matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_inf : Covariance matrix from information matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_inf_from_cov : Information matrix from covariance matrix
#' 
#' calculate_inf_from_corrse : Information matrix from correlation matrix and standard errors
#' 
#' 
#' @export
calculate_corr_from_inf <- function(information_matrix) {
	func_out <- pharmpy$modeling$calculate_corr_from_inf(information_matrix)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_cov_from_corrse
#' 
#' @description
#' Calculate covariance matrix from a correlation matrix and standard errors
#' 
#' @param corr (data.frame) Correlation matrix
#' @param se (data.frame) Standard errors
#'  
#' @return (data.frame) Covariance matrix
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' corr <- model$modelfit_results$correlation_matrix
#' se <- model$modelfit_results$standard_errors
#' corr
#' calculate_cov_from_corrse(corr, se)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_se_from_inf : Standard errors from information matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_inf : Covariance matrix from information matrix
#' 
#' calculate_inf_from_cov : Information matrix from covariance matrix
#' 
#' calculate_inf_from_corrse : Information matrix from correlation matrix and standard errors
#' 
#' calculate_corr_from_inf : Correlation matrix from information matrix
#' 
#' 
#' @export
calculate_cov_from_corrse <- function(corr, se) {
	func_out <- pharmpy$modeling$calculate_cov_from_corrse(corr, se)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_cov_from_inf
#' 
#' @description
#' Calculate covariance matrix from an information matrix
#' 
#' @param information_matrix (data.frame) Information matrix
#'  
#' @return (data.frame) Covariance matrix
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' inf <- model$modelfit_results$information_matrix
#' inf
#' calculate_cov_from_inf(inf)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_se_from_inf : Standard errors from information matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_inf_from_cov : Information matrix from covariance matrix
#' 
#' calculate_inf_from_corrse : Information matrix from correlation matrix and standard errors
#' 
#' calculate_corr_from_inf : Correlation matrix from information matrix
#' 
#' 
#' @export
calculate_cov_from_inf <- function(information_matrix) {
	func_out <- pharmpy$modeling$calculate_cov_from_inf(information_matrix)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_epsilon_gradient_expression
#' 
#' @description
#' Calculate the symbolic expression for the epsilon gradient
#' 
#' This function currently only support models without ODE systems
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Expression) Symbolic expression
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' calculate_epsilon_gradient_expression(model)
#' }
#' @seealso
#' calculate_eta_gradient_expression : Eta gradient
#' 
#' 
#' @export
calculate_epsilon_gradient_expression <- function(model) {
	func_out <- pharmpy$modeling$calculate_epsilon_gradient_expression(model)
	return(py_to_r(func_out))
}

#' @title
#' calculate_eta_gradient_expression
#' 
#' @description
#' Calculate the symbolic expression for the eta gradient
#' 
#' This function currently only support models without ODE systems
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Expression) Symbolic expression
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' calculate_eta_gradient_expression(model)
#' }
#' @seealso
#' calculate_epsilon_gradient_expression : Epsilon gradient
#' 
#' 
#' @export
calculate_eta_gradient_expression <- function(model) {
	func_out <- pharmpy$modeling$calculate_eta_gradient_expression(model)
	return(py_to_r(func_out))
}

#' @title
#' calculate_eta_shrinkage
#' 
#' @description
#' Calculate eta shrinkage for each eta
#' 
#' @param model (Model) Pharmpy model
#' @param parameter_estimates (data.frame) Parameter estimates
#' @param individual_estimates (data.frame) Table of individual (eta) estimates
#' @param sd (logical) Calculate shrinkage on the standard deviation scale (default is to calculate on the
#'  variance scale)
#'  
#' @return (Series) Shrinkage for each eta
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' pe <- model$modelfit_results$parameter_estimates
#' ie <- model$modelfit_results$individual_estimates
#' calculate_eta_shrinkage(model, pe, ie)
#' calculate_eta_shrinkage(model, pe, ie, sd=TRUE)
#' }
#' @seealso
#' calculate_individual_shrinkage
#' 
#' 
#' @export
calculate_eta_shrinkage <- function(model, parameter_estimates, individual_estimates, sd=FALSE) {
	func_out <- pharmpy$modeling$calculate_eta_shrinkage(model, parameter_estimates, individual_estimates, sd=sd)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_individual_parameter_statistics
#' 
#' @description
#' Calculate statistics for individual parameters
#' 
#' Calculate the mean (expected value of the distribution), variance
#' (variance of the distribution) and standard error for individual
#' parameters described by arbitrary expressions. Any dataset column or
#' variable used in the model can be used in the expression. The exception
#' being that variables that depends on the solution of the ODE system
#' cannot be used. If covariates are used in the expression the statistics
#' of the parameter is calculated at the median value of each covariate as well
#' as at the 5:th and 95:th percentiles. If no parameter uncertainty is available
#' for the model the standard error will not be calculated.
#' 
#' @param model (Model) A previously estimated model
#' @param parameter_estimates (data.frame) Parameter estimates
#' @param covariance_matrix (data.frame) Parameter uncertainty covariance matrix
#' @param expr_or_exprs (str, sympy expression or iterable of str or sympy expressions) Expressions or equations for parameters of interest. If equations are used
#'  the names of the left hand sides will be used as the names of the parameters.
#' @param rng (Generator or integer) Random number generator or integer seed
#'  
#' @return (data.frame) A DataFrame of statistics indexed on parameter and covariate value.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' rng <- create_rng(23)
#' pe <- model$modelfit_results$parameter_estimates
#' cov <- model$modelfit_results$covariance_matrix
#' calculate_individual_parameter_statistics(model, "K=CL/V", pe, cov, rng=rng)
#' }
#' 
#' @export
calculate_individual_parameter_statistics <- function(model, expr_or_exprs, parameter_estimates, covariance_matrix=NULL, rng=NULL) {
	func_out <- pharmpy$modeling$calculate_individual_parameter_statistics(model, expr_or_exprs, parameter_estimates, covariance_matrix=covariance_matrix, rng=rng)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_individual_shrinkage
#' 
#' @description
#' Calculate the individual eta-shrinkage
#' 
#' Definition: ieta_shr = (var(eta) / omega)
#' 
#' @param model (Model) Pharmpy model
#' @param parameter_estimates (data.frame) Parameter estimates of model
#' @param individual_estimates_covariance (data.frame) Uncertainty covariance matrices of individual estimates
#'  
#' @return (DataFrame) Shrinkage for each eta and individual
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' pe <- model$modelfit_results$parameter_estimates
#' covs <- model$modelfit_results$individual_estimates_covariance
#' calculate_individual_shrinkage(model, pe, covs)
#' }
#' @seealso
#' calculate_eta_shrinkage
#' 
#' 
#' @export
calculate_individual_shrinkage <- function(model, parameter_estimates, individual_estimates_covariance) {
	func_out <- pharmpy$modeling$calculate_individual_shrinkage(model, parameter_estimates, individual_estimates_covariance)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_inf_from_corrse
#' 
#' @description
#' Calculate information matrix from a correlation matrix and standard errors
#' 
#' @param corr (data.frame) Correlation matrix
#' @param se (data.frame) Standard errors
#'  
#' @return (data.frame) Information matrix
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' corr <- model$modelfit_results$correlation_matrix
#' se <- model$modelfit_results$standard_errors
#' corr
#' calculate_inf_from_corrse(corr, se)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_se_from_inf : Standard errors from information matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_inf : Covariance matrix from information matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_inf_from_cov : Information matrix from covariance matrix
#' 
#' calculate_corr_from_inf : Correlation matrix from information matrix
#' 
#' 
#' @export
calculate_inf_from_corrse <- function(corr, se) {
	func_out <- pharmpy$modeling$calculate_inf_from_corrse(corr, se)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_inf_from_cov
#' 
#' @description
#' Calculate information matrix from a covariance matrix
#' 
#' @param cov (data.frame) Covariance matrix
#'  
#' @return (data.frame) Information matrix
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' cov <- model$modelfit_results$covariance_matrix
#' cov
#' calculate_inf_from_cov(cov)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_se_from_inf : Standard errors from information matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_inf : Covariance matrix from information matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_inf_from_corrse : Information matrix from correlation matrix and standard errors
#' 
#' calculate_corr_from_inf : Correlation matrix from information matrix
#' 
#' 
#' @export
calculate_inf_from_cov <- function(cov) {
	func_out <- pharmpy$modeling$calculate_inf_from_cov(cov)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_parameters_from_ucp
#' 
#' @description
#' Scale parameter values from ucp to normal scale
#' 
#' @param model (Model) Pharmpy model
#' @param scale (UCPSCale) A parameter scale
#' @param ucps (data.frame or list) Series of parameter values
#'  
#' @return (data.frame) Parameters on the normal scale
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' scale <- calculate_ucp_scale(model)
#' values <- {'THETA(1)': 0.1, 'THETA(2)': 0.1, 'THETA(3)': 0.1,                   'OMEGA(1,1)': 0.1, 'OMEGA(2,2)': 0.1, 'SIGMA(1,1)': 0.1}
#' calculate_parameters_from_ucp(model, scale, values)
#' }
#' @seealso
#' calculate_ucp_scale : Calculate the scale for conversion from ucps
#' 
#' 
#' @export
calculate_parameters_from_ucp <- function(model, scale, ucps) {
	func_out <- pharmpy$modeling$calculate_parameters_from_ucp(model, scale, ucps)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_pk_parameters_statistics
#' 
#' @description
#' Calculate statistics for common pharmacokinetic parameters
#' 
#' Calculate the mean (expected value of the distribution), variance
#' (variance of the distribution) and standard error for some individual
#' pre-defined pharmacokinetic parameters.
#' 
#' @param model (Model) A previously estimated model
#' @param parameter_estimates (data.frame) Parameter estimates
#' @param covariance_matrix (data.frame) Parameter uncertainty covariance matrix
#' @param rng (Generator or integer) Random number generator or seed
#'  
#' @return (data.frame) A DataFrame of statistics indexed on parameter and covariate value.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' rng <- create_rng(23)
#' pe <- model$modelfit_results$parameter_estimates
#' cov <- model$modelfit_results$covariance_matrix
#' calculate_pk_parameters_statistics(model, pe, cov, rng=rng)
#' }
#' @seealso
#' calculate_individual_parameter_statistics : Calculation of statistics for arbitrary parameters
#' 
#' 
#' @export
calculate_pk_parameters_statistics <- function(model, parameter_estimates, covariance_matrix=NULL, rng=NULL) {
	func_out <- pharmpy$modeling$calculate_pk_parameters_statistics(model, parameter_estimates, covariance_matrix=covariance_matrix, rng=rng)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_se_from_cov
#' 
#' @description
#' Calculate standard errors from a covariance matrix
#' 
#' @param cov (data.frame) Input covariance matrix
#'  
#' @return (data.frame) Standard errors
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' cov <- model$modelfit_results$covariance_matrix
#' cov
#' calculate_se_from_cov(cov)
#' }
#' @seealso
#' calculate_se_from_inf : Standard errors from information matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_inf : Covariance matrix from information matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_inf_from_cov : Information matrix from covariance matrix
#' 
#' calculate_inf_from_corrse : Information matrix from correlation matrix and standard errors
#' 
#' calculate_corr_from_inf : Correlation matrix from information matrix
#' 
#' 
#' @export
calculate_se_from_cov <- function(cov) {
	func_out <- pharmpy$modeling$calculate_se_from_cov(cov)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_se_from_inf
#' 
#' @description
#' Calculate standard errors from an information matrix
#' 
#' @param information_matrix (data.frame) Input information matrix
#'  
#' @return (data.frame) Standard errors
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' inf <- model$modelfit_results$information_matrix
#' inf
#' calculate_se_from_inf(inf)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_inf : Covariance matrix from information matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_inf_from_cov : Information matrix from covariance matrix
#' 
#' calculate_inf_from_corrse : Information matrix from correlation matrix and standard errors
#' 
#' calculate_corr_from_inf : Correlation matrix from information matrix
#' 
#' 
#' @export
calculate_se_from_inf <- function(information_matrix) {
	func_out <- pharmpy$modeling$calculate_se_from_inf(information_matrix)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' calculate_ucp_scale
#' 
#' @description
#' Calculate a scale for unconstrained parameters for a model
#' 
#' The UCPScale object can be used to calculate unconstrained parameters
#' back into the normal parameter space.
#' 
#' @param model (Model) Model for which to calculate an ucp scale
#'  
#' @return (UCPScale) A scale object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' scale <- calculate_ucp_scale(model)
#' }
#' @seealso
#' calculate_parameters_from_ucp : Calculate parameters from ucp:s
#' 
#' 
#' 
#' 
#' @export
calculate_ucp_scale <- function(model) {
	func_out <- pharmpy$modeling$calculate_ucp_scale(model)
	return(py_to_r(func_out))
}

#' @title
#' check_dataset
#' 
#' @description
#' Check dataset for consistency across a set of rules
#' 
#' @param model (Model) Pharmpy model object
#' @param dataframe (Bool) TRUE to return a DataFrame instead of printing to the console
#' @param verbose (Bool) Print out all rules checked if TRUE else print only failed rules
#'  
#' @return (data.frame) Only returns a DataFrame is dataframe=TRUE
#' 
#' 
#' @export
check_dataset <- function(model, dataframe=FALSE, verbose=FALSE) {
	func_out <- pharmpy$modeling$check_dataset(model, dataframe=dataframe, verbose=verbose)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' check_high_correlations
#' 
#' @description
#' Check for highly correlated parameter estimates
#' 
#' @param model (Model) Pharmpy model object
#' @param cor (data.frame) Estimated correlation matrix
#' @param limit (numeric) Lower limit for a high correlation
#'  
#' @return (data.frame) Correlation values indexed on pairs of parameters for (absolute) correlations above limit
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' cor <- model$modelfit_results$correlation_matrix
#' check_high_correlations(model, cor, limit=0.3)
#' }
#' 
#' @export
check_high_correlations <- function(model, cor, limit=0.9) {
	func_out <- pharmpy$modeling$check_high_correlations(model, cor, limit=limit)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' check_parameters_near_bounds
#' 
#' @description
#' Check if any estimated parameter value is close to its bounds
#' 
#' @param model (Model) Pharmpy model object
#' @param values (data.frame) Series of values with index a subset of parameter names.
#' @param zero_limit (number) maximum distance to 0 bounds
#' @param significant_digits (integer) maximum distance to non-zero bounds in number of significant digits
#'  
#' @return (data.frame) Logical Series with same index as values
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' check_parameters_near_bounds(model, model$modelfit_results$parameter_estimates)
#' }
#' 
#' @export
check_parameters_near_bounds <- function(model, values, zero_limit=0.001, significant_digits=2) {
	func_out <- pharmpy$modeling$check_parameters_near_bounds(model, values, zero_limit=zero_limit, significant_digits=significant_digits)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' cleanup_model
#' 
#' @description
#' Perform various cleanups of a model
#' 
#' This is what is currently done
#' 
#' * Make model statements declarative, i.e. only one assignment per symbol
#' * Inline all assignments of one symbol, e.g. X = Y
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements
#' cleanup_model(model)
#' model$statements
#' }
#' @note
#' When creating NONMEM code from the cleaned model Pharmpy might need toadd certain assignments to make it in line with what NONMEM requires.
#' 
#' 
#' @export
cleanup_model <- function(model) {
	func_out <- pharmpy$modeling$cleanup_model(model)
	return(py_to_r(func_out))
}

#' @title
#' convert_model
#' 
#' @description
#' Convert model to other format
#' 
#' Note that the operation is not done inplace.
#' 
#' @param model (Model) Model to convert
#' @param to_format (str) Name of format to convert into. Currently supported 'generic', 'nlmixr' and 'nonmem'
#'  
#' @return (Model) New model object with new underlying model format
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' converted_model <- convert_model(model, "nlmixr")
#' }
#' 
#' @export
convert_model <- function(model, to_format) {
	func_out <- pharmpy$modeling$convert_model(model, to_format)
	return(py_to_r(func_out))
}

#' @title
#' copy_model
#' 
#' @description
#' Copies model to a new model object
#' 
#' @param model (Model) Pharmpy model
#' @param name (str) Optional new name of model
#'  
#' @return (Model) A copy of the input model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model_copy <- copy_model(model, "pheno2")
#' }
#' 
#' @export
copy_model <- function(model, name=NULL) {
	func_out <- pharmpy$modeling$copy_model(model, name=name)
	return(py_to_r(func_out))
}

#' @title
#' create_joint_distribution
#' 
#' @description
#' Combines some or all etas into a joint distribution.
#' 
#' The etas must be IIVs and cannot
#' be fixed. Initial estimates for covariance between the etas is dependent on whether
#' the model has results from a previous run. In that case, the correlation will
#' be calculated from individual estimates, otherwise correlation will be set to 10%.
#' 
#' @param model (Model) Pharmpy model
#' @param rvs (vector) Sequence of etas or names of etas to combine. If NULL, all etas that are IIVs and
#'  non-fixed will be used (full block). NULL is default.
#' @param individual_estimates (data.frame) Optional individual estimates to use for calculation of initial estimates
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$random_variables$etas
#' create_joint_distribution(model, c('ETA(1)', 'ETA(2)'))
#' model$random_variables$etas
#' }
#' @seealso
#' split_joint_distribution : split etas into separate distributions
#' 
#' 
#' @export
create_joint_distribution <- function(model, rvs=NULL, individual_estimates=NULL) {
	func_out <- pharmpy$modeling$create_joint_distribution(model, rvs=rvs, individual_estimates=individual_estimates)
	return(py_to_r(func_out))
}

#' @title
#' create_report
#' 
#' @description
#' Create standard report for results
#' 
#' The report will be an html created at specified path.
#' 
#' @param results (Results) Results for which to create report
#' @param path (Path) Path to report file
#' 
#' @export
create_report <- function(results, path) {
	func_out <- pharmpy$modeling$create_report(results, path)
	return(py_to_r(func_out))
}

#' @title
#' create_rng
#' 
#' @description
#' Create a new random number generator
#' 
#' Pharmpy functions that use random sampling take a random number generator or seed as input.
#' This function can be used to create a default new random number generator.
#' 
#' @param seed (integer or rng) Seed for the random number generator or NULL (default) for a randomized seed. If seed
#'  is generator it will be passed through.
#'  
#' @return (Generator) Initialized numpy random number generator object
#' 
#' @examples
#' \dontrun{
#' rng <- create_rng(23)
#' rng$standard_normal()
#' }
#' 
#' @export
create_rng <- function(seed=NULL) {
	func_out <- pharmpy$modeling$create_rng(seed=seed)
	return(py_to_r(func_out))
}

#' @title
#' create_symbol
#' 
#' @description
#' Create a new unique variable symbol given a model
#' 
#' @param model (Model) Pharmpy model object
#' @param stem (str) First part of the new variable name
#' @param force_numbering (logical) Forces addition of number to name even if variable does not exist, e.g.
#'  COVEFF --> COVEFF1
#'  
#' @return (Symbol) Created symbol with unique name
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' create_symbol(model, "TEMP")
#' create_symbol(model, "TEMP", force_numbering=TRUE)
#' create_symbol(model, "CL")
#' }
#' 
#' @export
create_symbol <- function(model, stem, force_numbering=FALSE) {
	func_out <- pharmpy$modeling$create_symbol(model, stem, force_numbering=force_numbering)
	return(py_to_r(func_out))
}

#' @title
#' deidentify_data
#' 
#' @description
#' Deidentify a dataset
#' 
#' Two operations are performed on the dataset:
#' 
#' 1. All ID numbers are randomized from the range 1 to n
#' 2. All columns containing dates will have the year changed
#' 
#' The year change is done by letting the earliest year in the dataset
#' be used as a reference and by maintaining leap years. The reference year
#' will either be 1901, 1902, 1903 or 1904 depending on its distance to the closest
#' preceeding leap year.
#' 
#' @param df (data.frame) A dataset
#' @param id_column (str) Name of the id column
#' @param date_columns (vector) Names of all date columns
#'  
#' @return (data.frame) Deidentified dataset
#' 
#' 
#' @export
deidentify_data <- function(df, id_column='ID', date_columns=NULL) {
	func_out <- pharmpy$modeling$deidentify_data(df, id_column=id_column, date_columns=date_columns)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' drop_columns
#' 
#' @description
#' Drop columns from the dataset or mark as dropped
#' 
#' @param model (Model) Pharmpy model object
#' @param column_names (vector or str) List of column names or one column name to drop or mark as dropped
#' @param mark (logical) Default is to remove column from dataset. Set this to TRUE to only mark as dropped
#'  
#' @return (Model) Reference to same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' drop_columns(model, c('WGT', 'APGR'))
#' vector(model$dataset$columns)
#' }
#' @seealso
#' drop_dropped_columns : Drop all columns marked as drop
#' 
#' undrop_columns : Undrop columns of model
#' 
#' 
#' @export
drop_columns <- function(model, column_names, mark=FALSE) {
	func_out <- pharmpy$modeling$drop_columns(model, column_names, mark=mark)
	return(py_to_r(func_out))
}

#' @title
#' drop_dropped_columns
#' 
#' @description
#' Drop columns marked as dropped from the dataset
#' 
#' NM-TRAN date columns will not be dropped by this function
#' even if marked as dropped.
#' Columns not specified in the datainfo ($INPUT for NONMEM)
#' will also be dropped from the dataset.
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Model) Reference to same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' drop_dropped_columns(model)
#' vector(model$dataset$columns)
#' }
#' @seealso
#' drop_columns : Drop specific columns or mark them as drop
#' 
#' 
#' @export
drop_dropped_columns <- function(model) {
	func_out <- pharmpy$modeling$drop_dropped_columns(model)
	return(py_to_r(func_out))
}

#' @title
#' evaluate_epsilon_gradient
#' 
#' @description
#' Evaluate the numeric epsilon gradient
#' 
#' The gradient is evaluated at the current model parameter values
#' or optionally at the given parameter values.
#' The gradient is done for each data record in the model dataset
#' or optionally using the dataset argument.
#' The gradient is done at the current eta values
#' or optionally at the given eta values.
#' 
#' This function currently only support models without ODE systems
#' 
#' @param model (Model) Pharmpy model
#' @param etas (list) Optional list of eta values
#' @param parameters (list) Optional list of parameters and values
#' @param dataset (data.frame) Optional dataset
#'  
#' @return (data.frame) Gradient
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' etas <- model$modelfit_results$individual_estimates
#' evaluate_epsilon_gradient(model, etas=etas)
#' }
#' @seealso
#' evaluate_eta_gradient : Evaluate the eta gradient
#' 
#' 
#' @export
evaluate_epsilon_gradient <- function(model, etas=NULL, parameters=NULL, dataset=NULL) {
	func_out <- pharmpy$modeling$evaluate_epsilon_gradient(model, etas=etas, parameters=parameters, dataset=dataset)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' evaluate_eta_gradient
#' 
#' @description
#' Evaluate the numeric eta gradient
#' 
#' The gradient is evaluated at the current model parameter values
#' or optionally at the given parameter values.
#' The gradient is done for each data record in the model dataset
#' or optionally using the dataset argument.
#' The gradient is done at the current eta values
#' or optionally at the given eta values.
#' 
#' This function currently only support models without ODE systems
#' 
#' @param model (Model) Pharmpy model
#' @param etas (list) Optional list of eta values
#' @param parameters (list) Optional list of parameters and values
#' @param dataset (data.frame) Optional dataset
#'  
#' @return (data.frame) Gradient
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' etas <- model$modelfit_results$individual_estimates
#' evaluate_eta_gradient(model, etas=etas)
#' }
#' @seealso
#' evaluate_epsilon_gradient : Evaluate the epsilon gradient
#' 
#' 
#' @export
evaluate_eta_gradient <- function(model, etas=NULL, parameters=NULL, dataset=NULL) {
	func_out <- pharmpy$modeling$evaluate_eta_gradient(model, etas=etas, parameters=parameters, dataset=dataset)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' evaluate_expression
#' 
#' @description
#' Evaluate expression using model
#' 
#' Calculate the value of expression for each data record.
#' The expression can contain dataset columns, variables in model and
#' population parameters. If the model has parameter estimates these
#' will be used. Initial estimates will be used for non-estimated parameters.
#' 
#' @param model (Model) Pharmpy model
#' @param expression (str or sympy expression) Expression to evaluate
#' @param parameter_estimates (data.frame) Parameter estimates to use instead of initial estimates
#'  
#' @return (data.frame) A series of one evaluated value for each data record
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' pe <- model$modelfit_results$parameter_estimates
#' evaluate_expression(model, "TVCL*1000", parameter_estimates=pe)
#' }
#' 
#' @export
evaluate_expression <- function(model, expression, parameter_estimates=NULL) {
	func_out <- pharmpy$modeling$evaluate_expression(model, expression, parameter_estimates=parameter_estimates)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' evaluate_individual_prediction
#' 
#' @description
#' Evaluate the numeric individual prediction
#' 
#' The prediction is evaluated at the current model parameter values
#' or optionally at the given parameter values.
#' The evaluation is done for each data record in the model dataset
#' or optionally using the dataset argument.
#' The evaluation is done at the current eta values
#' or optionally at the given eta values.
#' 
#' This function currently only support models without ODE systems
#' 
#' @param model (Model) Pharmpy model
#' @param etas (list) Optional list of eta values
#' @param parameters (list) Optional list of parameters and values
#' @param dataset (data.frame) Optional dataset
#'  
#' @return (data.frame) Individual predictions
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' etas <- model$modelfit_results$individual_estimates
#' evaluate_individual_prediction(model, etas=etas)
#' }
#' @seealso
#' evaluate_population_prediction : Evaluate the population prediction
#' 
#' 
#' @export
evaluate_individual_prediction <- function(model, etas=NULL, parameters=NULL, dataset=NULL) {
	func_out <- pharmpy$modeling$evaluate_individual_prediction(model, etas=etas, parameters=parameters, dataset=dataset)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' evaluate_population_prediction
#' 
#' @description
#' Evaluate the numeric population prediction
#' 
#' The prediction is evaluated at the current model parameter values
#' or optionally at the given parameter values.
#' The evaluation is done for each data record in the model dataset
#' or optionally using the dataset argument.
#' 
#' This function currently only support models without ODE systems
#' 
#' @param model (Model) Pharmpy model
#' @param parameters (list) Optional list of parameters and values
#' @param dataset (data.frame) Optional dataset
#'  
#' @return (data.frame) Population predictions
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' pe <- model$modelfit_results$parameter_estimates
#' evaluate_population_prediction(model, parameters=list(pe))
#' }
#' @seealso
#' evaluate_individual_prediction : Evaluate the individual prediction
#' 
#' 
#' @export
evaluate_population_prediction <- function(model, parameters=NULL, dataset=NULL) {
	func_out <- pharmpy$modeling$evaluate_population_prediction(model, parameters=parameters, dataset=dataset)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' evaluate_weighted_residuals
#' 
#' @description
#' Evaluate the weighted residuals
#' 
#' The residuals is evaluated at the current model parameter values
#' or optionally at the given parameter values.
#' The residuals is done for each data record in the model dataset
#' or optionally using the dataset argument.
#' 
#' This function currently only support models without ODE systems
#' 
#' @param model (Model) Pharmpy model
#' @param parameters (list) Optional list of parameters and values
#' @param dataset (data.frame) Optional dataset
#'  
#' @return (data.frame) WRES
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' parameters <- model$modelfit_results$parameter_estimates
#' evaluate_weighted_residuals(model, parameters=list(parameters))
#' }
#' 
#' @export
evaluate_weighted_residuals <- function(model, parameters=NULL, dataset=NULL) {
	func_out <- pharmpy$modeling$evaluate_weighted_residuals(model, parameters=parameters, dataset=dataset)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' expand_additional_doses
#' 
#' @description
#' Expand additional doses into separate dose records
#' 
#' @param model (Model) Pharmpy model object
#' @param flag (logical) TRUE to add a boolean EXPANDED column to mark added records. In this case all
#'  columns in the original dataset will be kept. Care needs to be taken to handle
#'  the new dataset.
#'  
#' @return (Model) Reference to the same model object
#' 
#' 
#' @export
expand_additional_doses <- function(model, flag=FALSE) {
	func_out <- pharmpy$modeling$expand_additional_doses(model, flag=flag)
	return(py_to_r(func_out))
}

#' @title
#' find_clearance_parameters
#' 
#' @description
#' Find clearance parameters in model
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (vector) A vector of clearance parameters
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' find_clearance_parameters(model)
#' }
#' 
#' @export
find_clearance_parameters <- function(model) {
	func_out <- pharmpy$modeling$find_clearance_parameters(model)
	return(py_to_r(func_out))
}

#' @title
#' find_volume_parameters
#' 
#' @description
#' Find volume parameters in model
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (vector) A vector of volume parameters
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' find_volume_parameters(model)
#' }
#' 
#' @export
find_volume_parameters <- function(model) {
	func_out <- pharmpy$modeling$find_volume_parameters(model)
	return(py_to_r(func_out))
}

#' @title
#' fix_or_unfix_parameters
#' 
#' @description
#' Fix or unfix parameters
#' 
#' Set fixedness of parameters to specified values
#' 
#' @param model (Model) Pharmpy model
#' @param parameters (list) Set fix/unfix for these parameters
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$parameters['THETA(1)']
#' fix_or_unfix_parameters(model, list('THETA(1)'=TRUE))
#' model$parameters['THETA(1)']
#' }
#' @seealso
#' fix_parameters : Fix parameters
#' 
#' unfix_paramaters : Unfixing parameters
#' 
#' fix_paramaters_to : Fixing parameters and setting a new initial estimate in the same
#' 
#' function
#' 
#' unfix_paramaters_to : Unfixing parameters and setting a new initial estimate in the same
#' 
#' function
#' 
#' 
#' @export
fix_or_unfix_parameters <- function(model, parameters) {
	func_out <- pharmpy$modeling$fix_or_unfix_parameters(model, parameters)
	return(py_to_r(func_out))
}

#' @title
#' fix_parameters
#' 
#' @description
#' Fix parameters
#' 
#' Fix all listed parameters
#' 
#' @param model (Model) Pharmpy model
#' @param parameter_names (vector or str) one parameter name or a vector of parameter names
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$parameters['THETA(1)']
#' fix_parameters(model, 'THETA(1)')
#' model$parameters['THETA(1)']
#' }
#' @seealso
#' fix_or_unfix_parameters : Fix or unfix parameters (given boolean)
#' 
#' fix_parameters_to : Fixing and setting parameter initial estimates in the same function
#' 
#' unfix_paramaters : Unfixing parameters
#' 
#' unfix_paramaters_to : Unfixing parameters and setting a new initial estimate in the same
#' 
#' function
#' 
#' 
#' @export
fix_parameters <- function(model, parameter_names) {
	func_out <- pharmpy$modeling$fix_parameters(model, parameter_names)
	return(py_to_r(func_out))
}

#' @title
#' fix_parameters_to
#' 
#' @description
#' Fix parameters to
#' 
#' Fix all listed parameters to specified value/values
#' 
#' @param model (Model) Pharmpy model
#' @param inits (list) Inits for all parameters to fix and set init
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$parameters['THETA(1)']
#' fix_parameters_to(model, {'THETA(1)': 0.5})
#' model$parameters['THETA(1)']
#' }
#' @seealso
#' fix_parameters : Fix parameters
#' 
#' fix_or_unfix_parameters : Fix or unfix parameters (given boolean)
#' 
#' unfix_paramaters : Unfixing parameters
#' 
#' unfix_paramaters_to : Unfixing parameters and setting a new initial estimate in the same
#' 
#' function
#' 
#' 
#' @export
fix_parameters_to <- function(model, inits) {
	func_out <- pharmpy$modeling$fix_parameters_to(model, inits)
	return(py_to_r(func_out))
}

#' @title
#' generate_model_code
#' 
#' @description
#' Get the model code of the underlying model language
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (str) Model code
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' generate_model_code(model)
#' }
#' 
#' @export
generate_model_code <- function(model) {
	func_out <- pharmpy$modeling$generate_model_code(model)
	return(py_to_r(func_out))
}

#' @title
#' get_baselines
#' 
#' @description
#' Baselines for each subject.
#' 
#' Baseline is taken to be the first row even if that has a missing value.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (data.frame) Dataset with the baselines
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_baselines(model)
#' }
#' 
#' @export
get_baselines <- function(model) {
	func_out <- pharmpy$modeling$get_baselines(model)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' get_bioavailability
#' 
#' @description
#' Get bioavailability of doses for all compartments
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (list) Dictionary from compartment name to bioavailability expression
#' 
#' 
#' @export
get_bioavailability <- function(model) {
	func_out <- pharmpy$modeling$get_bioavailability(model)
	return(py_to_r(func_out))
}

#' @title
#' get_cmt
#' 
#' @description
#' Get the cmt (compartment) column from the model dataset
#' 
#' If a cmt column is present this will be extracted otherwise
#' a cmt column will be created.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (data.frame) CMT
#' 
#' 
#' @export
get_cmt <- function(model) {
	func_out <- pharmpy$modeling$get_cmt(model)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' get_concentration_parameters_from_data
#' 
#' @description
#' Create a dataframe with concentration parameters
#' 
#' Note that all values are directly calculated from the dataset
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (data.frame) Concentration parameters
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_concentration_parameters_from_data(model)
#' }
#' 
#' @export
get_concentration_parameters_from_data <- function(model) {
	func_out <- pharmpy$modeling$get_concentration_parameters_from_data(model)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' get_config_path
#' 
#' @description
#' Returns path to the user config path
#' 
#' @return (str or NULL) Path to user config or NULL if file does not exist
#' 
#' @examples
#' \dontrun{
#' get_config_path()
#' }
#' 
#' @export
get_config_path <- function() {
	func_out <- pharmpy$modeling$get_config_path()
	return(py_to_r(func_out))
}

#' @title
#' get_covariate_baselines
#' 
#' @description
#' Return a dataframe with baselines of all covariates for each id.
#' 
#' Baseline is taken to be the first row even if that has a missing value.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (data.frame) covariate baselines
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_covariates(model, c("WGT", "APGR"))
#' get_covariate_baselines(model)
#' }
#' @seealso
#' get_baselines : baselines for all data columns
#' 
#' 
#' 
#' 
#' @export
get_covariate_baselines <- function(model) {
	func_out <- pharmpy$modeling$get_covariate_baselines(model)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' get_doseid
#' 
#' @description
#' Get a DOSEID series from the dataset with an id of each dose period starting from 1
#' 
#' If a a dose and observation exist at the same time point the observation will be counted
#' towards the previous dose.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (data.frame) DOSEIDs
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_doseid(model)
#' }
#' 
#' @export
get_doseid <- function(model) {
	func_out <- pharmpy$modeling$get_doseid(model)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' get_doses
#' 
#' @description
#' Get a series of all doses
#' 
#' Indexed with ID and TIME
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (data.frame) doses
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_doses(model)
#' }
#' 
#' @export
get_doses <- function(model) {
	func_out <- pharmpy$modeling$get_doses(model)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' get_evid
#' 
#' @description
#' Get the evid from model dataset
#' 
#' If an event column is present this will be extracted otherwise
#' an evid column will be created.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (data.frame) EVID
#' 
#' 
#' @export
get_evid <- function(model) {
	func_out <- pharmpy$modeling$get_evid(model)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' get_ids
#' 
#' @description
#' Retrieve a vector of all subject ids of the dataset
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (vector) All subject ids
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_ids(model)
#' }
#' 
#' @export
get_ids <- function(model) {
	func_out <- pharmpy$modeling$get_ids(model)
	return(py_to_r(func_out))
}

#' @title
#' get_individual_parameters
#' 
#' @description
#' Retrieves all parameters with IIV or IOV in :class:`pharmpy.model`.
#' 
#' @param model (Model) Pharmpy model to retrieve the individuals parameters from
#' @param level (str) The variability level to look for: 'iiv', 'iov', or 'all' (default)
#'  
#' @return (vector[str]) A vector of the parameter names as strings
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_individual_parameters(model)
#' get_individual_parameters(model, 'iiv')
#' get_individual_parameters(model, 'iov')
#' }
#' @seealso
#' get_pk_parameters
#' 
#' get_rv_parameters
#' 
#' has_random_effect
#' 
#' 
#' @export
get_individual_parameters <- function(model, level='all') {
	func_out <- pharmpy$modeling$get_individual_parameters(model, level=level)
	return(py_to_r(func_out))
}

#' @title
#' get_individual_prediction_expression
#' 
#' @description
#' Get the full symbolic expression for the modelled individual prediction
#' 
#' This function currently only support models without ODE systems
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Expression) Symbolic expression
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' get_individual_prediction_expression(model)
#' }
#' @seealso
#' get_population_prediction_expression : Get full symbolic epression for the population prediction
#' 
#' 
#' @export
get_individual_prediction_expression <- function(model) {
	func_out <- pharmpy$modeling$get_individual_prediction_expression(model)
	return(py_to_r(func_out))
}

#' @title
#' get_lag_times
#' 
#' @description
#' Get lag times for all compartments
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (list) Dictionary from compartment name to lag time expression
#' 
#' 
#' @export
get_lag_times <- function(model) {
	func_out <- pharmpy$modeling$get_lag_times(model)
	return(py_to_r(func_out))
}

#' @title
#' get_mdv
#' 
#' @description
#' Get MDVs from dataset
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (data.frame) MDVs
#' 
#' 
#' @export
get_mdv <- function(model) {
	func_out <- pharmpy$modeling$get_mdv(model)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' get_model_covariates
#' 
#' @description
#' List of covariates used in model
#' 
#' A covariate in the model is here defined to be a data item
#' affecting the model prediction excluding dosing items that
#' are not used in model code.
#' 
#' @param model (Model) Pharmpy model
#' @param strings (logical) Return strings instead of symbols? FALSE (default) will give symbols
#'  
#' @return (vector) Covariate symbols or names
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_model_covariates(model)
#' get_model_covariates(model, strings=TRUE)
#' }
#' 
#' @export
get_model_covariates <- function(model, strings=FALSE) {
	func_out <- pharmpy$modeling$get_model_covariates(model, strings=strings)
	return(py_to_r(func_out))
}

#' @title
#' get_number_of_individuals
#' 
#' @description
#' Retrieve the number of individuals in the model dataset
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (integer) Number of individuals in the model dataset
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_number_of_individuals(model)
#' }
#' @note
#' For NONMEM models this is the number of individuals of the active dataset, i.e. after filteringof IGNORE and ACCEPT and removal of individuals with no observations.
#' 
#' @seealso
#' get_number_of_observations : Get the number of observations in a dataset
#' 
#' get_number_of_observations_per_individual : Get the number of observations per individual in a
#' 
#' dataset
#' 
#' 
#' @export
get_number_of_individuals <- function(model) {
	func_out <- pharmpy$modeling$get_number_of_individuals(model)
	return(py_to_r(func_out))
}

#' @title
#' get_number_of_observations
#' 
#' @description
#' Retrieve the total number of observations in the model dataset
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (integer) Number of observations in the model dataset
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_number_of_observations(model)
#' }
#' @note
#' For NONMEM models this is the number of observations of the active dataset, i.e. after filteringof IGNORE and ACCEPT and removal of individuals with no observations.
#' 
#' @seealso
#' get_number_of_individuals : Get the number of individuals in a dataset
#' 
#' get_number_of_observations_per_individual : Get the number of observations per individual in a
#' 
#' dataset
#' 
#' 
#' @export
get_number_of_observations <- function(model) {
	func_out <- pharmpy$modeling$get_number_of_observations(model)
	return(py_to_r(func_out))
}

#' @title
#' get_number_of_observations_per_individual
#' 
#' @description
#' Number of observations for each individual
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (data.frame) Number of observations in the model dataset
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_number_of_observations_per_individual(model)
#' }
#' @note
#' For NONMEM models this is the individuals and number of observations of the active dataset, i.e.after filtering of IGNORE and ACCEPT and removal of individuals with no observations.
#' 
#' @seealso
#' get_number_of_individuals : Get the number of individuals in a dataset
#' 
#' get_number_of_observations_per_individual : Get the number of observations per individual in a
#' 
#' dataset
#' 
#' 
#' @export
get_number_of_observations_per_individual <- function(model) {
	func_out <- pharmpy$modeling$get_number_of_observations_per_individual(model)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' get_observation_expression
#' 
#' @description
#' Get the full symbolic expression for the observation according to the model
#' 
#' This function currently only support models without ODE systems
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Expression) Symbolic expression
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' expr <- get_observation_expression(model)
#' sympy$pprint(expr)
#' }
#' 
#' @export
get_observation_expression <- function(model) {
	func_out <- pharmpy$modeling$get_observation_expression(model)
	return(py_to_r(func_out))
}

#' @title
#' get_observations
#' 
#' @description
#' Get observations from dataset
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (data.frame) Observations indexed over ID and TIME
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_observations(model)
#' }
#' @seealso
#' get_number_of_observations : get the number of observations
#' 
#' get_number_of_observations_per_individual : get the number of observations per individual
#' 
#' 
#' @export
get_observations <- function(model) {
	func_out <- pharmpy$modeling$get_observations(model)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' get_omegas
#' 
#' @description
#' Get all omegas (variability parameters) of a model
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Parameters) A copy of all omega parameters
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_omegas(model)
#' }
#' @seealso
#' get_thetas : Get theta parameters
#' 
#' get_sigmas : Get sigma parameters
#' 
#' 
#' @export
get_omegas <- function(model) {
	func_out <- pharmpy$modeling$get_omegas(model)
	return(py_to_r(func_out))
}

#' @title
#' get_pk_parameters
#' 
#' @description
#' Retrieves PK parameters in :class:`pharmpy.model`.
#' 
#' @param model (Model) Pharmpy model to retrieve the PK parameters from
#' @param kind (str) The type of parameter to retrieve: 'absorption', 'distribution',
#'  'elimination', or 'all' (default).
#'  
#' @return (vector[str]) A vector of the PK parameter names of the given model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_pk_parameters(model)
#' get_pk_parameters(model, 'absorption')
#' get_pk_parameters(model, 'distribution')
#' get_pk_parameters(model, 'elimination')
#' }
#' @seealso
#' get_individual_parameters
#' 
#' get_rv_parameters
#' 
#' 
#' @export
get_pk_parameters <- function(model, kind='all') {
	func_out <- pharmpy$modeling$get_pk_parameters(model, kind=kind)
	return(py_to_r(func_out))
}

#' @title
#' get_population_prediction_expression
#' 
#' @description
#' Get the full symbolic expression for the modelled population prediction
#' 
#' This function currently only support models without ODE systems
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Expression) Symbolic expression
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' get_population_prediction_expression(model)
#' }
#' @seealso
#' get_individual_prediction_expression : Get full symbolic epression for the individual prediction
#' 
#' 
#' @export
get_population_prediction_expression <- function(model) {
	func_out <- pharmpy$modeling$get_population_prediction_expression(model)
	return(py_to_r(func_out))
}

#' @title
#' get_rv_parameters
#' 
#' @description
#' Retrieves parameters in :class:`pharmpy.model` given a random variable.
#' 
#' @param model (Model) Pharmpy model to retrieve parameters from
#' @param rv (str) Name of random variable to retrieve
#'  
#' @return (vector[str]) A vector of parameter names for the given random variable
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_rv_parameters(model, 'ETA(1)')
#' }
#' @seealso
#' has_random_effect
#' 
#' get_pk_parameters
#' 
#' get_individual_parameters
#' 
#' 
#' @export
get_rv_parameters <- function(model, rv) {
	func_out <- pharmpy$modeling$get_rv_parameters(model, rv)
	return(py_to_r(func_out))
}

#' @title
#' get_sigmas
#' 
#' @description
#' Get all sigmas (residual error variability parameters) of a model
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Parameters) A copy of all sigma parameters
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_sigmas(model)
#' }
#' @seealso
#' get_thetas : Get theta parameters
#' 
#' get_omegas : Get omega parameters
#' 
#' 
#' @export
get_sigmas <- function(model) {
	func_out <- pharmpy$modeling$get_sigmas(model)
	return(py_to_r(func_out))
}

#' @title
#' get_thetas
#' 
#' @description
#' Get all thetas (structural parameters) of a model
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Parameters) A copy of all theta parameters
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_thetas(model)
#' }
#' @seealso
#' get_omegas : Get omega parameters
#' 
#' get_sigmas : Get sigma parameters
#' 
#' 
#' @export
get_thetas <- function(model) {
	func_out <- pharmpy$modeling$get_thetas(model)
	return(py_to_r(func_out))
}

#' @title
#' get_unit_of
#' 
#' @description
#' Derive the physical unit of a variable in the model
#' 
#' Unit information for the dataset needs to be available.
#' The variable can be defined in the code, a dataset olumn, a parameter
#' or a random variable.
#' 
#' @param model (Model) Pharmpy model object
#' @param variable (str or Symbol) Find physical unit of this variable
#'  
#' @return (unit expression) A sympy physics.units expression
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_unit_of(model, "Y")
#' get_unit_of(model, "V")
#' get_unit_of(model, "WGT")
#' }
#' 
#' @export
get_unit_of <- function(model, variable) {
	func_out <- pharmpy$modeling$get_unit_of(model, variable)
	return(py_to_r(func_out))
}

#' @title
#' greekify_model
#' 
#' @description
#' Convert to using greek letters for all population parameters
#' 
#' @param model (Model) Pharmpy model
#' @param named_subscripts (logical) Use previous parameter names as subscripts. Default is to use integer subscripts
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements
#' greekify_model(cleanup_model(model))
#' model$statements
#' }
#' 
#' @export
greekify_model <- function(model, named_subscripts=FALSE) {
	func_out <- pharmpy$modeling$greekify_model(model, named_subscripts=named_subscripts)
	return(py_to_r(func_out))
}

#' @title
#' has_additive_error_model
#' 
#' @description
#' Check if a model has an additive error model
#' 
#' @param model (Model) The model to check
#'  
#' @return (logical) TRUE if the model has an additive error model and FALSE otherwise
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_additive_error_model(model)
#' }
#' @seealso
#' has_proportional_error_model : Check if a model has a proportional error model
#' 
#' has_combined_error_model : Check if a model has a combined error model
#' 
#' 
#' @export
has_additive_error_model <- function(model) {
	func_out <- pharmpy$modeling$has_additive_error_model(model)
	return(py_to_r(func_out))
}

#' @title
#' has_combined_error_model
#' 
#' @description
#' Check if a model has a combined additive and proportinal error model
#' 
#' @param model (Model) The model to check
#'  
#' @return (logical) TRUE if the model has a combined error model and FALSE otherwise
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_combined_error_model(model)
#' }
#' @seealso
#' has_additive_error_model : Check if a model has an additive error model
#' 
#' has_proportional_error_model : Check if a model has a proportional error model
#' 
#' 
#' @export
has_combined_error_model <- function(model) {
	func_out <- pharmpy$modeling$has_combined_error_model(model)
	return(py_to_r(func_out))
}

#' @title
#' has_covariate_effect
#' 
#' @description
#' Tests if an instance of :class:`pharmpy.model` has a given covariate
#' effect.
#' 
#' @param model (Model) Pharmpy model to check for covariate effect.
#' @param parameter (str) Name of parameter.
#' @param covariate (str) Name of covariate.
#'  
#' @return (logical) Whether input model has a covariate effect of the input covariate on the input parameter.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_covariate_effect(model, "CL", "APGR")
#' }
#' 
#' @export
has_covariate_effect <- function(model, parameter, covariate) {
	func_out <- pharmpy$modeling$has_covariate_effect(model, parameter, covariate)
	return(py_to_r(func_out))
}

#' @title
#' has_first_order_elimination
#' 
#' @description
#' Check if the model describes first order elimination
#' 
#' This function relies on heuristics and will not be able to detect all
#' possible ways of coding the first order elimination.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (logical) TRUE if model has describes first order elimination
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_first_order_elimination(model)
#' }
#' 
#' @export
has_first_order_elimination <- function(model) {
	func_out <- pharmpy$modeling$has_first_order_elimination(model)
	return(py_to_r(func_out))
}

#' @title
#' has_michaelis_menten_elimination
#' 
#' @description
#' Check if the model describes Michaelis-Menten elimination
#' 
#' This function relies on heuristics and will not be able to detect all
#' possible ways of coding the Michaelis-Menten elimination.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (logical) TRUE if model has describes Michaelis-Menten elimination
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_michaelis_menten_elimination(model)
#' set_michaelis_menten_elimination(model)
#' has_michaelis_menten_elimination(model)
#' }
#' 
#' @export
has_michaelis_menten_elimination <- function(model) {
	func_out <- pharmpy$modeling$has_michaelis_menten_elimination(model)
	return(py_to_r(func_out))
}

#' @title
#' has_mixed_mm_fo_elimination
#' 
#' @description
#' Check if the model describes mixed Michaelis-Menten and first order elimination
#' 
#' This function relies on heuristics and will not be able to detect all
#' possible ways of coding the mixed Michalis-Menten and first order elimination.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (logical) TRUE if model has describes Michaelis-Menten elimination
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_mixed_mm_fo_elimination(model)
#' set_mixed_mm_fo_elimination(model)
#' has_mixed_mm_fo_elimination(model)
#' }
#' 
#' @export
has_mixed_mm_fo_elimination <- function(model) {
	func_out <- pharmpy$modeling$has_mixed_mm_fo_elimination(model)
	return(py_to_r(func_out))
}

#' @title
#' has_proportional_error_model
#' 
#' @description
#' Check if a model has a proportional error model
#' 
#' @param model (Model) The model to check
#'  
#' @return (logical) TRUE if the model has a proportional error model and FALSE otherwise
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_proportional_error_model(model)
#' }
#' @seealso
#' has_additive_error_model : Check if a model has an additive error model
#' 
#' has_combined_error_model : Check if a model has a combined error model
#' 
#' 
#' @export
has_proportional_error_model <- function(model) {
	func_out <- pharmpy$modeling$has_proportional_error_model(model)
	return(py_to_r(func_out))
}

#' @title
#' has_random_effect
#' 
#' @description
#' Decides whether the given parameter of a :class:`pharmpy.model` has a
#' random effect.
#' 
#' @param model (Model) Input Pharmpy model
#' @param parameter (str) Input parameter
#' @param level (str) The variability level to look for: 'iiv', 'iov', or 'all' (default)
#'  
#' @return (logical) Whether the given parameter has a random effect
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_random_effect(model, 'S1')
#' has_random_effect(model, 'CL', 'iiv')
#' has_random_effect(model, 'CL', 'iov')
#' }
#' @seealso
#' get_individual_parameters
#' 
#' get_rv_parameters
#' 
#' 
#' @export
has_random_effect <- function(model, parameter, level='all') {
	func_out <- pharmpy$modeling$has_random_effect(model, parameter, level=level)
	return(py_to_r(func_out))
}

#' @title
#' has_zero_order_absorption
#' 
#' @description
#' Check if ode system describes a zero order absorption
#' 
#' currently defined as having Infusion dose with rate not in dataset
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_zero_order_absorption(model)
#' }
#' 
#' @export
has_zero_order_absorption <- function(model) {
	func_out <- pharmpy$modeling$has_zero_order_absorption(model)
	return(py_to_r(func_out))
}

#' @title
#' has_zero_order_elimination
#' 
#' @description
#' Check if the model describes zero-order elimination
#' 
#' This function relies on heuristics and will not be able to detect all
#' possible ways of coding the zero-order elimination.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (logical) TRUE if model has describes zero order elimination
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_zero_order_elimination(model)
#' set_zero_order_elimination(model)
#' has_zero_order_elimination(model)
#' }
#' 
#' @export
has_zero_order_elimination <- function(model) {
	func_out <- pharmpy$modeling$has_zero_order_elimination(model)
	return(py_to_r(func_out))
}

#' @title
#' list_time_varying_covariates
#' 
#' @description
#' Return a vector of names of all time varying covariates
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (vector) Names of all time varying covariates
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' list_time_varying_covariates(model)
#' }
#' @seealso
#' get_covariate_baselines : get baselines for all covariates
#' 
#' 
#' 
#' 
#' @export
list_time_varying_covariates <- function(model) {
	func_out <- pharmpy$modeling$list_time_varying_covariates(model)
	return(py_to_r(func_out))
}

#' @title
#' load_example_model
#' 
#' @description
#' Load an example model
#' 
#' Load an example model from models built into Pharmpy
#' 
#' @param name (str) Name of the model. Currently available models are "pheno" and "pheno_linear"
#'  
#' @return (Model) Loaded model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements
#' }
#' 
#' @export
load_example_model <- function(name) {
	func_out <- pharmpy$modeling$load_example_model(name)
	return(py_to_r(func_out))
}

#' @title
#' make_declarative
#' 
#' @description
#' Make the model statments declarative
#' 
#' Each symbol will only be declared once.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements$before_odes
#' make_declarative(model)
#' model$statements$before_odes
#' }
#' 
#' @export
make_declarative <- function(model) {
	func_out <- pharmpy$modeling$make_declarative(model)
	return(py_to_r(func_out))
}

#' @title
#' mu_reference_model
#' 
#' @description
#' Convert model to use mu-referencing
#' 
#' Mu-referencing an eta is to separately define its actual mu (mean) parameter.
#' For example: :math:`CL = \theta_1 e^{\eta_1}` with :math:`\eta_1` following a zero-mean
#' normal distribution would give :math:`\mu_1 = log{\theta_1}` and
#' :math:`CL = e^{\mu_1 + \eta_1}`
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Model) Reference to same object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' mu_reference_model(model).statements$before_odes
#' }
#' 
#' @export
mu_reference_model <- function(model) {
	func_out <- pharmpy$modeling$mu_reference_model(model)
	return(py_to_r(func_out))
}

#' @title
#' omit_data
#' 
#' @description
#' Iterate over omissions of a certain group in a dataset. One group is omitted at a time.
#' 
#' @param dataset_or_model (data.frame or Model) Dataset or model for which to omit records
#' @param group (str) Name of the column to use for grouping
#' @param name_pattern (str) Name to use for generated datasets. A number starting from 1 will be put in the placeholder.
#'  
#' @return (iterator) Iterator yielding tuples of models/dataframes and the omited group
#' 
#' 
#' @export
omit_data <- function(dataset_or_model, group, name_pattern='omitted_{}') {
	func_out <- pharmpy$modeling$omit_data(dataset_or_model, group, name_pattern=name_pattern)
	return(py_to_r(func_out))
}

#' @title
#' plot_individual_predictions
#' 
#' @description
#' Plot DV and predictions grouped on individuals
#' 
#' @param model (Model) Previously run Pharmpy model.
#' @param predictions (data.frame) One column for each type of prediction
#' @param individuals (vector) A vector of individuals to include. NULL for all individuals
#'  
#' @return (alt.Chart) Plot
#' 
#' 
#' @export
plot_individual_predictions <- function(model, predictions, individuals=NULL) {
	func_out <- pharmpy$modeling$plot_individual_predictions(model, predictions, individuals=individuals)
	return(py_to_r(func_out))
}

#' @title
#' plot_iofv_vs_iofv
#' 
#' @description
#' Plot individual OFV of two models against each other
#' 
#' @param iofv1 (data.frame) Estimated iOFV of the first model
#' @param iofv2 (data.frame) Estimated iOFV of the second model
#' @param name1 (str) Name of first model
#' @param name2 (str) Name of second model
#'  
#' @return (alt.Chart) Scatterplot
#' 
#' 
#' @export
plot_iofv_vs_iofv <- function(iofv1, iofv2, name1, name2) {
	func_out <- pharmpy$modeling$plot_iofv_vs_iofv(iofv1, iofv2, name1, name2)
	return(py_to_r(func_out))
}

#' @title
#' print_model_code
#' 
#' @description
#' Print the model code of the underlying model language
#' 
#' @param model (Model) Pharmpy model
#'  
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' print_model_code(model)
#' }
#' 
#' @export
print_model_code <- function(model) {
	func_out <- pharmpy$modeling$print_model_code(model)
	return(py_to_r(func_out))
}

#' @title
#' print_model_symbols
#' 
#' @description
#' Print all symbols defined in a model
#' 
#' Symbols will be in one of the categories thetas, etas, omegas, epsilons, sigmas,
#' variables and data columns
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' print_model_symbols(model)
#' }
#' 
#' @export
print_model_symbols <- function(model) {
	func_out <- pharmpy$modeling$print_model_symbols(model)
	return(py_to_r(func_out))
}

#' @title
#' read_dataset_from_datainfo
#' 
#' @description
#' Read a dataset given a datainfo object or path to a datainfo file
#' 
#' @param datainfo (DataInfo | Path | str) A datainfo object or a path to a datainfo object
#' @param datatype (str) A string to specify dataset type
#'  
#' @return (data.frame) The dataset
#' 
#' 
#' @export
read_dataset_from_datainfo <- function(datainfo, datatype=NULL) {
	func_out <- pharmpy$modeling$read_dataset_from_datainfo(datainfo, datatype=datatype)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' read_model
#' 
#' @description
#' Read model from file
#' 
#' @param path (str or Path) Path to model
#'  
#' @return (Model) Read model object
#' 
#' @examples
#' \dontrun{
#' model <- read_model("/home/run1$mod")
#' }
#' @seealso
#' read_model_from_database : Read model from database
#' 
#' read_model_from_string : Read model from string
#' 
#' 
#' @export
read_model <- function(path) {
	func_out <- pharmpy$modeling$read_model(path)
	return(py_to_r(func_out))
}

#' @title
#' read_model_from_database
#' 
#' @description
#' Read model from model database
#' 
#' @param name (str) Name of model to use as lookup
#' @param database (Database) Database to use. Will use default database if not specified.
#'  
#' @return (Model) Read model object
#' 
#' @examples
#' \dontrun{
#' model <- read_model_from_database("run1")
#' }
#' @seealso
#' read_model : Read model from file
#' 
#' read_model_from_string : Read model from string
#' 
#' 
#' @export
read_model_from_database <- function(name, database=NULL) {
	func_out <- pharmpy$modeling$read_model_from_database(name, database=database)
	return(py_to_r(func_out))
}

#' @title
#' read_model_from_string
#' 
#' @description
#' Read model from the model code in a string
#' 
#' @param code (str) Model code to read
#' @param path (Path or str) Specified to set the path for the created model
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' s <- "$PROBLEM
#' $INPUT ID DV TIME
#' $DATA file$csv
#' $PRED
#' Y=THETA(1)+ETA(1)+ERR(1)
#' $THETA 1
#' $OMEGA 0.1
#' $SIGMA 1
#' $ESTIMATION METHOD=1"
#' read_model_from_string(s)
#' }
#' @seealso
#' read_model : Read model from file
#' 
#' read_model_from_database : Read model from database
#' 
#' 
#' @export
read_model_from_string <- function(code, path=NULL) {
	func_out <- pharmpy$modeling$read_model_from_string(code, path=path)
	return(py_to_r(func_out))
}

#' @title
#' remove_covariance_step
#' 
#' @description
#' Removes covariance step to the final estimation step
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' remove_covariance_step(model)
#' ests <- model$estimation_steps
#' ests[1]
#' }
#' @seealso
#' add_estimation_step
#' 
#' set_estimation_step
#' 
#' remove_estimation_step
#' 
#' append_estimation_step_options
#' 
#' add_covariance_step
#' 
#' set_evaluation_step
#' 
#' 
#' @export
remove_covariance_step <- function(model) {
	func_out <- pharmpy$modeling$remove_covariance_step(model)
	return(py_to_r(func_out))
}

#' @title
#' remove_covariate_effect
#' 
#' @description
#' Remove a covariate effect from an instance of :class:`pharmpy.model`.
#' 
#' @param model (Model) Pharmpy model from which to remove the covariate effect.
#' @param parameter (str) Name of parameter.
#' @param covariate (str) Name of covariate.
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_covariate_effect(model, "CL", "WGT")
#' remove_covariate_effect(model, "CL", "WGT")
#' has_covariate_effect(model, "CL", "WGT")
#' }
#' 
#' @export
remove_covariate_effect <- function(model, parameter, covariate) {
	func_out <- pharmpy$modeling$remove_covariate_effect(model, parameter, covariate)
	return(py_to_r(func_out))
}

#' @title
#' remove_error_model
#' 
#' @description
#' Remove error model.
#' 
#' @param model (Model) Remove error model for this model
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements$find_assignment("Y")
#' remove_error_model(model)
#' model$statements$find_assignment("Y")
#' }
#' 
#' @export
remove_error_model <- function(model) {
	func_out <- pharmpy$modeling$remove_error_model(model)
	return(py_to_r(func_out))
}

#' @title
#' remove_estimation_step
#' 
#' @description
#' Remove estimation step
#' 
#' @param model (Model) Pharmpy model
#' @param idx (integer) index of estimation step to remove (starting from 0)
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' remove_estimation_step(model, 0)
#' ests <- model$estimation_steps
#' length(ests)
#' }
#' @seealso
#' add_estimation_step
#' 
#' set_estimation_step
#' 
#' append_estimation_step_options
#' 
#' add_covariance_step
#' 
#' remove_covariance_step
#' 
#' set_evaluation_step
#' 
#' 
#' @export
remove_estimation_step <- function(model, idx) {
	func_out <- pharmpy$modeling$remove_estimation_step(model, idx)
	return(py_to_r(func_out))
}

#' @title
#' remove_iiv
#' 
#' @description
#' Removes all IIV etas given a vector with eta names and/or parameter names.
#' 
#' @param model (Model) Pharmpy model to create block effect on.
#' @param to_remove (str, vector) Name/names of etas and/or name/names of individual parameters to remove.
#'  If NULL, all etas that are IIVs will be removed. NULL is default.
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' remove_iiv(model)
#' model$statements$find_assignment("CL")
#' model <- load_example_model("pheno")
#' remove_iiv(model, "V")
#' model$statements$find_assignment("V")
#' }
#' @seealso
#' remove_iov
#' 
#' add_iiv
#' 
#' add_iov
#' 
#' add_pk_iiv
#' 
#' 
#' @export
remove_iiv <- function(model, to_remove=NULL) {
	func_out <- pharmpy$modeling$remove_iiv(model, to_remove=to_remove)
	return(py_to_r(func_out))
}

#' @title
#' remove_iov
#' 
#' @description
#' Removes all IOV etas given a vector with eta names.
#' 
#' @param model (Model) Pharmpy model to remove IOV from.
#' @param to_remove (str, vector) Name/names of IOV etas to remove, e.g. 'ETA_IOV_1_1'.
#'  If NULL, all etas that are IOVs will be removed. NULL is default.
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' remove_iov(model)
#' }
#' @seealso
#' add_iiv
#' 
#' add_iov
#' 
#' remove_iiv
#' 
#' add_pk_iiv
#' 
#' 
#' @export
remove_iov <- function(model, to_remove=NULL) {
	func_out <- pharmpy$modeling$remove_iov(model, to_remove=to_remove)
	return(py_to_r(func_out))
}

#' @title
#' remove_lag_time
#' 
#' @description
#' Remove lag time from the dose compartment of model.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' remove_lag_time(model)
#' }
#' @seealso
#' set_transit_compartments
#' 
#' add_lag_time
#' 
#' 
#' @export
remove_lag_time <- function(model) {
	func_out <- pharmpy$modeling$remove_lag_time(model)
	return(py_to_r(func_out))
}

#' @title
#' remove_loq_data
#' 
#' @description
#' Remove loq data records from the dataset
#' 
#' Does nothing if none of the limits is specified.
#' 
#' @param model (Model) Pharmpy model object
#' @param lloq (numeric) Lower limit of quantification. Default not specified.
#' @param uloq (numeric) Upper limit of quantification. Default not specified.
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' remove_loq_data(model, lloq=10, uloq=40)
#' length(model$dataset)
#' }
#' 
#' @export
remove_loq_data <- function(model, lloq=NULL, uloq=NULL) {
	func_out <- pharmpy$modeling$remove_loq_data(model, lloq=lloq, uloq=uloq)
	return(py_to_r(func_out))
}

#' @title
#' remove_peripheral_compartment
#' 
#' @description
#' Remove a peripheral distribution compartment from model
#' 
#' Initial estimates:
#' 
#' ==  ===================================================
#' n
#' ==  ===================================================
#' 2   :math:`{CL} = {CL'}`,
#' :math:`{QP1} = {CL'}` and :math:`{VP1} = {VC'} * 0.05`
#' 3   :math:`{QP1} = ({QP1'} + {QP2'}) / 2`,
#' :math:`{VP1} = {VP1'} + {VP2'}`
#' ==  ===================================================
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_peripheral_compartments(model, 2)
#' remove_peripheral_compartment(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_peripheral_compartment
#' 
#' add_peripheral_compartment
#' 
#' 
#' @export
remove_peripheral_compartment <- function(model) {
	func_out <- pharmpy$modeling$remove_peripheral_compartment(model)
	return(py_to_r(func_out))
}

#' @title
#' remove_unused_parameters_and_rvs
#' 
#' @description
#' Remove any parameters and rvs that are not used in the model statements
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Model) Reference to same model object
#' 
#' 
#' @export
remove_unused_parameters_and_rvs <- function(model) {
	func_out <- pharmpy$modeling$remove_unused_parameters_and_rvs(model)
	return(py_to_r(func_out))
}

#' @title
#' rename_symbols
#' 
#' @description
#' Rename symbols in the model
#' 
#' Make sure that no name clash occur.
#' 
#' @param model (Model) Pharmpy model object
#' @param new_names (list) From old name or symbol to new name or symbol
#'  
#' @return (Model) Reference to same model object
#' 
#' 
#' @export
rename_symbols <- function(model, new_names) {
	func_out <- pharmpy$modeling$rename_symbols(model, new_names)
	return(py_to_r(func_out))
}

#' @title
#' resample_data
#' 
#' @description
#' Iterate over resamples of a dataset.
#' 
#' The dataset will be grouped on the group column then groups will be selected
#' randomly with or without replacement to form a new dataset.
#' The groups will be renumbered from 1 and upwards to keep them separated in the new
#' dataset.
#' 
#' @param dataset_or_model (data.frame or Model) Dataset or Model to use
#' @param group (str) Name of column to group by
#' @param resamples (integer) Number of resamples (iterations) to make
#' @param stratify (str) Name of column to use for stratification.
#'  The values in the stratification column must be equal within a group so that the group
#'  can be uniquely determined. A ValueError exception will be raised otherwise.
#' @param sample_size (integer) The number of groups that should be sampled. The default is
#'  the number of groups. If using stratification the default is to sample using the
#'  proportion of the stratas in the dataset. A list of specific sample sizes
#'  for each strata can also be supplied.
#' @param replace (logical) A boolean controlling whether sampling should be done with or
#'  without replacement
#' @param name_pattern (str) Name to use for generated datasets. A number starting from 1 will
#'  be put in the placeholder.
#' @param name (str) Option to name pattern in case of only one resample
#'  
#' @return (iterator) An iterator yielding tuples of a resampled DataFrame and a vector of resampled groups in order
#' 
#' 
#' @export
resample_data <- function(dataset_or_model, group, resamples=1, stratify=NULL, sample_size=NULL, replace=FALSE, name_pattern='resample_{}', name=NULL) {
	func_out <- pharmpy$modeling$resample_data(dataset_or_model, group, resamples=resamples, stratify=stratify, sample_size=sample_size, replace=replace, name_pattern=name_pattern, name=name)
	return(py_to_r(func_out))
}

#' @title
#' sample_individual_estimates
#' 
#' @description
#' Sample individual estimates given their covariance.
#' 
#' @param model (Model) Pharmpy model
#' @param individual_estimates (data.frame) Individual estimates to use
#' @param individual_estimates_covariance (data.frame) Uncertainty covariance of the individual estimates
#' @param parameters (vector) A vector of a subset of individual parameters to sample. Default is NULL, which means all.
#' @param samples_per_id (integer) Number of samples per individual
#' @param rng (rng or integer) Random number generator or seed
#'  
#' @return (data.frame) Pool of samples in a DataFrame
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' rng <- create_rng(23)
#' ie <- model$modelfit_results$individual_estimates
#' iec <- model$modelfit_results$individual_estimates_covariance
#' sample_individual_estimates(model, ie, iec, samples_per_id=2, rng=rng)
#' }
#' @seealso
#' sample_parameters_from_covariance_matrix : Sample parameter vectors using the
#' 
#' uncertainty covariance matrix
#' 
#' sample_parameters_uniformly : Sample parameter vectors using uniform distribution
#' 
#' 
#' @export
sample_individual_estimates <- function(model, individual_estimates, individual_estimates_covariance, parameters=NULL, samples_per_id=100, rng=NULL) {
	func_out <- pharmpy$modeling$sample_individual_estimates(model, individual_estimates, individual_estimates_covariance, parameters=parameters, samples_per_id=samples_per_id, rng=rng)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' sample_parameters_from_covariance_matrix
#' 
#' @description
#' Sample parameter vectors using the covariance matrix
#' 
#' If parameters is not provided all estimated parameters will be used
#' 
#' @param model (Model) Input model
#' @param parameter_estimates (data.frame) Parameter estimates to use as means in sampling
#' @param covariance_matrix (data.frame) Parameter uncertainty covariance matrix
#' @param force_posdef_samples (integer) Set to how many iterations to do before forcing all samples to be positive definite. NULL is
#'  default and means never and 0 means always
#' @param force_posdef_covmatrix (logical) Set to TRUE to force the input covariance matrix to be positive definite
#' @param n (integer) Number of samples
#' @param rng (Generator) Random number generator
#'  
#' @return (data.frame) A dataframe with one sample per row
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' rng <- create_rng(23)
#' cov <- model$modelfit_results$covariance_matrix
#' pe <- model$modelfit_results$parameter_estimates
#' sample_parameters_from_covariance_matrix(model, pe, cov, n=3, rng=rng)
#' }
#' @seealso
#' sample_parameters_uniformly : Sample parameter vectors using uniform distribution
#' 
#' sample_individual_estimates : Sample individual estiates given their covariance
#' 
#' 
#' @export
sample_parameters_from_covariance_matrix <- function(model, parameter_estimates, covariance_matrix, force_posdef_samples=NULL, force_posdef_covmatrix=FALSE, n=1, rng=NULL) {
	func_out <- pharmpy$modeling$sample_parameters_from_covariance_matrix(model, parameter_estimates, covariance_matrix, force_posdef_samples=force_posdef_samples, force_posdef_covmatrix=force_posdef_covmatrix, n=n, rng=rng)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' sample_parameters_uniformly
#' 
#' @description
#' Sample parameter vectors using uniform sampling
#' 
#' Each parameter value will be randomly sampled from a uniform distribution
#' with the bounds being estimate ± estimate * fraction.
#' 
#' @param model (Model) Pharmpy model
#' @param parameter_estimates (data.frame) Parameter estimates for parameters to use
#' @param fraction (numeric) Fraction of estimate value to use for distribution bounds
#' @param force_posdef_samples (integer) Number of samples to reject before forcing variability parameters to give
#'  positive definite covariance matrices.
#' @param n (integer) Number of samples
#' @param rng (integer or rng) Random number generator or seed
#'  
#' @return (data.frame) samples
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' rng <- create_rng(23)
#' pe <- model$modelfit_results$parameter_estimates
#' sample_parameters_uniformly(model, pe, n=3, rng=rng)
#' }
#' @seealso
#' sample_parameters_from_covariance_matrix : Sample parameter vectors using the
#' 
#' uncertainty covariance matrix
#' 
#' sample_individual_estimates : Sample individual estiates given their covariance
#' 
#' 
#' @export
sample_parameters_uniformly <- function(model, parameter_estimates, fraction=0.1, force_posdef_samples=NULL, n=1, rng=NULL) {
	func_out <- pharmpy$modeling$sample_parameters_uniformly(model, parameter_estimates, fraction=fraction, force_posdef_samples=force_posdef_samples, n=n, rng=rng)
	if (func_out$index$nlevels > 1) {
		func_out <- func_out$reset_index()
	}
	return(py_to_r(func_out))
}

#' @title
#' set_additive_error_model
#' 
#' @description
#' Set an additive error model. Initial estimate for new sigma is :math:`(min(DV)/2)²`.
#' 
#' The error function being applied depends on the data transformation. The table displays
#' some examples.
#' 
#' +------------------------+----------------------------------------+
#' | Data transformation    | Additive error                         |
#' +========================+========================================+
#' | :math:`y`              | :math:`f + epsilon_1`                 |
#' +------------------------+----------------------------------------+
#' | :math:`log(y)`         | :math:`log(f) + frac{epsilon_1}{f}` |
#' +------------------------+----------------------------------------+
#' 
#' @param model (Model) Set error model for this model
#' @param data_trans (str or expression) A data transformation expression or NULL (default) to use the transformation
#'  specified by the model. Series expansion will be used for approximation.
#' @param series_terms (integer) Number of terms to use for the series expansion approximation for data
#'  transformation.
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements$find_assignment("Y")
#' set_additive_error_model(model)
#' model$statements$find_assignment("Y")
#' model <- load_example_model("pheno")
#' model$statements$find_assignment("Y")
#' set_additive_error_model(model, data_trans="log(Y)")
#' model$statements$find_assignment("Y")
#' }
#' @seealso
#' set_proportional_error_model : Proportional error model
#' 
#' set_combined_error_model : Combined error model
#' 
#' 
#' @export
set_additive_error_model <- function(model, data_trans=NULL, series_terms=2) {
	func_out <- pharmpy$modeling$set_additive_error_model(model, data_trans=data_trans, series_terms=series_terms)
	return(py_to_r(func_out))
}

#' @title
#' set_bolus_absorption
#' 
#' @description
#' Set or change to bolus absorption rate.
#' 
#' Currently lagtime together with bolus absorption is not supported.
#' 
#' @param model (Model) Model to set or change absorption rate
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_bolus_absorption(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_zero_order_absorption
#' 
#' set_first_order_absorption
#' 
#' 
#' @export
set_bolus_absorption <- function(model) {
	func_out <- pharmpy$modeling$set_bolus_absorption(model)
	return(py_to_r(func_out))
}

#' @title
#' set_combined_error_model
#' 
#' @description
#' Set a combined error model. Initial estimates for new sigmas are :math:`(min(DV)/2)²` for
#' proportional and 0.09 for additive.
#' 
#' The error function being applied depends on the data transformation.
#' 
#' +------------------------+-----------------------------------------------------+
#' | Data transformation    | Combined error                                      |
#' +========================+=====================================================+
#' | :math:`y`              | :math:`f + f epsilon_1 + epsilon_2`               |
#' +------------------------+-----------------------------------------------------+
#' | :math:`log(y)`         | :math:`log(f) + epsilon_1 + frac{epsilon_2}{f}` |
#' +------------------------+-----------------------------------------------------+
#' 
#' @param model (Model) Set error model for this model
#' @param data_trans (str or expression) A data transformation expression or NULL (default) to use the transformation
#'  specified by the model.
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- remove_error_model(load_example_model("pheno"))
#' set_combined_error_model(model)
#' model$statements$find_assignment("Y")
#' model <- remove_error_model(load_example_model("pheno"))
#' set_combined_error_model(model, data_trans="log(Y)")
#' model$statements$find_assignment("Y")
#' }
#' @seealso
#' set_additive_error_model : Additive error model
#' 
#' set_proportional_error_model: Proportional error model
#' 
#' 
#' @export
set_combined_error_model <- function(model, data_trans=NULL) {
	func_out <- pharmpy$modeling$set_combined_error_model(model, data_trans=data_trans)
	return(py_to_r(func_out))
}

#' @title
#' set_covariates
#' 
#' @description
#' Set columns in the dataset to be covariates in the datainfo
#' 
#' @param model (Model) Pharmpy model
#' @param covariates (vector) List of column names
#'  
#' @return (Model) Reference to the same Pharmpy model object
#' 
#' 
#' @export
set_covariates <- function(model, covariates) {
	func_out <- pharmpy$modeling$set_covariates(model, covariates)
	return(py_to_r(func_out))
}

#' @title
#' set_dtbs_error_model
#' 
#' @description
#' Dynamic transform both sides
#' 
#' @param model (Model) Pharmpy model
#' @param fix_to_log (Boolean) Set to TRUE to fix lambda and zeta to 0, i.e. emulating log-transformed data
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_dtbs_error_model(model)
#' }
#' 
#' @export
set_dtbs_error_model <- function(model, fix_to_log=FALSE) {
	func_out <- pharmpy$modeling$set_dtbs_error_model(model, fix_to_log=fix_to_log)
	return(py_to_r(func_out))
}

#' @title
#' set_estimation_step
#' 
#' @description
#' Set estimation step
#' 
#' Sets estimation step for a model. Methods currently supported are:
#' FO, FOCE, ITS, LAPLACE, IMPMAP, IMP, SAEM, BAYES
#' 
#' @param model (Model) Pharmpy model
#' @param method (str) estimation method to change to
#' @param idx (integer) index of estimation step, default is 0 (first estimation step)
#' @param ... Arguments to pass to EstimationStep (such as interaction, evaluation)
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' opts <- list('NITER'=1000, 'ISAMPLE'=100)
#' set_estimation_step(model, "IMP", evaluation=TRUE, tool_options=opts)
#' model$estimation_steps[1]
#' }
#' @seealso
#' add_estimation_step
#' 
#' remove_estimation_step
#' 
#' append_estimation_step_options
#' 
#' add_covariance_step
#' 
#' remove_covariance_step
#' 
#' set_evaluation_step
#' 
#' 
#' @export
set_estimation_step <- function(model, method, idx=0, ...) {
	func_out <- pharmpy$modeling$set_estimation_step(model, method, idx=idx, ...)
	return(py_to_r(func_out))
}

#' @title
#' set_evaluation_step
#' 
#' @description
#' Set estimation step
#' 
#' Sets estimation step for a model. Methods currently supported are:
#' FO, FOCE, ITS, LAPLACE, IMPMAP, IMP, SAEM, BAYES
#' 
#' @param model (Model) Pharmpy model
#' @param idx (integer) index of estimation step, default is -1 (last estimation step)
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_evaluation_step(model)
#' model$estimation_steps[1]
#' }
#' @seealso
#' set_estimation_step
#' 
#' add_estimation_step
#' 
#' remove_estimation_step
#' 
#' append_estimation_step_options
#' 
#' add_covariance_step
#' 
#' remove_covariance_step
#' 
#' 
#' @export
set_evaluation_step <- function(model, idx=-1) {
	func_out <- pharmpy$modeling$set_evaluation_step(model, idx=idx)
	return(py_to_r(func_out))
}

#' @title
#' set_first_order_absorption
#' 
#' @description
#' Set or change to first order absorption rate.
#' 
#' Initial estimate for absorption rate is set to
#' the previous rate if available, otherwise it is set to the time of first observation/2.
#' 
#' @param model (Model) Model to set or change to use first order absorption rate
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_first_order_absorption(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_bolus_order_absorption
#' 
#' set_zero_order_absorption
#' 
#' 
#' @export
set_first_order_absorption <- function(model) {
	func_out <- pharmpy$modeling$set_first_order_absorption(model)
	return(py_to_r(func_out))
}

#' @title
#' set_first_order_elimination
#' 
#' @description
#' Sets elimination to first order
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_first_order_elimination(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_zero_order_elimination
#' 
#' set_michaelis_menten_elimination
#' 
#' 
#' @export
set_first_order_elimination <- function(model) {
	func_out <- pharmpy$modeling$set_first_order_elimination(model)
	return(py_to_r(func_out))
}

#' @title
#' set_iiv_on_ruv
#' 
#' @description
#' Multiplies epsilons with exponential (new) etas.
#' 
#' Initial variance for new etas is 0.09.
#' 
#' @param model (Model) Pharmpy model to apply IIV on epsilons.
#' @param list_of_eps (str, vector) Name/names of epsilons to multiply with exponential etas. If NULL, all epsilons will
#'  be chosen. NULL is default.
#' @param same_eta (logical) Boolean of whether all RUVs from input should use the same new ETA or if one ETA
#'  should be created for each RUV. TRUE is default.
#' @param eta_names (str, vector) Custom names of new etas. Must be equal to the number epsilons or 1 if same eta.
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_iiv_on_ruv(model)
#' model$statements$find_assignment("Y")
#' }
#' @seealso
#' set_power_on_ruv
#' 
#' 
#' @export
set_iiv_on_ruv <- function(model, list_of_eps=NULL, same_eta=TRUE, eta_names=NULL) {
	func_out <- pharmpy$modeling$set_iiv_on_ruv(model, list_of_eps=list_of_eps, same_eta=same_eta, eta_names=eta_names)
	return(py_to_r(func_out))
}

#' @title
#' set_initial_estimates
#' 
#' @description
#' Set initial estimates
#' 
#' @param model (Model) Pharmpy model
#' @param inits (list) A list of parameter init for parameters to change
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_initial_estimates(model, list('THETA(1)'=2))
#' model$parameters['THETA(1)']
#' }
#' @seealso
#' fix_parameters_to : Fixing and setting parameter initial estimates in the same function
#' 
#' unfix_paramaters_to : Unfixing parameters and setting a new initial estimate in the same
#' 
#' 
#' @export
set_initial_estimates <- function(model, inits) {
	func_out <- pharmpy$modeling$set_initial_estimates(model, inits)
	return(py_to_r(func_out))
}

#' @title
#' set_lower_bounds
#' 
#' @description
#' Set parameter lower bounds
#' 
#' @param model (Model) Pharmpy model
#' @param bounds (list) A list of parameter bounds for parameters to change
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_lower_bounds(model, {'THETA(1)': -10})
#' model$parameters['THETA(1)']
#' }
#' @seealso
#' set_upper_bounds : Set parameter upper bounds
#' 
#' unconstrain_parameters : Remove all constraints of parameters
#' 
#' 
#' @export
set_lower_bounds <- function(model, bounds) {
	func_out <- pharmpy$modeling$set_lower_bounds(model, bounds)
	return(py_to_r(func_out))
}

#' @title
#' set_michaelis_menten_elimination
#' 
#' @description
#' Sets elimination to Michaelis-Menten.
#' 
#' Initial estimate for CLMM is set to CL and KM is set to :math:`2*max(DV)`.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_michaelis_menten_elimination(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_first_order_elimination
#' 
#' set_zero_order_elimination
#' 
#' 
#' @export
set_michaelis_menten_elimination <- function(model) {
	func_out <- pharmpy$modeling$set_michaelis_menten_elimination(model)
	return(py_to_r(func_out))
}

#' @title
#' set_mixed_mm_fo_elimination
#' 
#' @description
#' Sets elimination to mixed Michaelis-Menten and first order.
#' 
#' Initial estimate for CLMM is set to CL/2 and KM is set to :math:`2*max(DV)`.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_mixed_mm_fo_elimination(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_first_order_elimination
#' 
#' set_zero_order_elimination
#' 
#' set_michaelis_menten_elimination
#' 
#' 
#' @export
set_mixed_mm_fo_elimination <- function(model) {
	func_out <- pharmpy$modeling$set_mixed_mm_fo_elimination(model)
	return(py_to_r(func_out))
}

#' @title
#' set_name
#' 
#' @description
#' Set name of model object
#' 
#' @param model (Model) Pharmpy model
#' @param new_name (str) New name of model
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$name
#' set_name(model, "run2")
#' model$name
#' }
#' 
#' @export
set_name <- function(model, new_name) {
	func_out <- pharmpy$modeling$set_name(model, new_name)
	return(py_to_r(func_out))
}

#' @title
#' set_ode_solver
#' 
#' @description
#' Sets ODE solver to use for model
#' 
#' Recognized solvers and their corresponding NONMEM advans:
#' 
#' +----------------------------+------------------+
#' | Solver                     | NONMEM ADVAN     |
#' +============================+==================+
#' | CVODES                     | ADVAN14          |
#' +----------------------------+------------------+
#' | DGEAR                      | ADVAN8           |
#' +----------------------------+------------------+
#' | DVERK                      | ADVAN6           |
#' +----------------------------+------------------+
#' | IDA                        | ADVAN15          |
#' +----------------------------+------------------+
#' | LSODA                      | ADVAN13          |
#' +----------------------------+------------------+
#' | LSODI                      | ADVAN9           |
#' +----------------------------+------------------+
#' 
#' @param model (Model) Pharmpy model
#' @param solver (str) Solver to use or NULL for no preference
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_ode_solver(model, 'LSODA')
#' }
#' 
#' @export
set_ode_solver <- function(model, solver) {
	func_out <- pharmpy$modeling$set_ode_solver(model, solver)
	return(py_to_r(func_out))
}

#' @title
#' set_peripheral_compartments
#' 
#' @description
#' Sets the number of peripheral compartments to a specified number.
#' 
#' @param model (Model) Pharmpy model
#' @param n (integer) Number of transit compartments
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_peripheral_compartments(model, 2)
#' model$statements$ode_system
#' }
#' @seealso
#' add_peripheral_compartment
#' 
#' remove_peripheral_compartment
#' 
#' 
#' @export
set_peripheral_compartments <- function(model, n) {
	func_out <- pharmpy$modeling$set_peripheral_compartments(model, n)
	return(py_to_r(func_out))
}

#' @title
#' set_power_on_ruv
#' 
#' @description
#' Applies a power effect to provided epsilons.
#' 
#' Initial estimates for new thetas are 1 if the error
#' model is proportional, otherwise they are 0.1.
#' 
#' @param model (Model) Pharmpy model to create block effect on.
#' @param list_of_eps (str or vector or NULL) Name/names of epsilons to apply power effect. If NULL, all epsilons will be used.
#'  NULL is default.
#' @param lower_limit (numeric or NULL) Lower limit of power (theta). NULL for no limit.
#' @param ipred (Symbol) Symbol to use as IPRED. Default is to autodetect expression for IPRED.
#' @param zero_protection (logical) Set to TRUE to add code protecting from IPRED=0
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_power_on_ruv(model)
#' model$statements$find_assignment("Y")
#' }
#' @seealso
#' set_iiv_on_ruv
#' 
#' 
#' @export
set_power_on_ruv <- function(model, list_of_eps=NULL, lower_limit=0.01, ipred=NULL, zero_protection=FALSE) {
	func_out <- pharmpy$modeling$set_power_on_ruv(model, list_of_eps=list_of_eps, lower_limit=lower_limit, ipred=ipred, zero_protection=zero_protection)
	return(py_to_r(func_out))
}

#' @title
#' set_proportional_error_model
#' 
#' @description
#' Set a proportional error model. Initial estimate for new sigma is 0.09.
#' 
#' The error function being applied depends on the data transformation.
#' 
#' +------------------------+----------------------------------------+
#' | Data transformation    | Proportional error                     |
#' +========================+========================================+
#' | :math:`y`              | :math:`f + f epsilon_1`               |
#' +------------------------+----------------------------------------+
#' | :math:`log(y)`         | :math:`log(f) + epsilon_1`           |
#' +------------------------+----------------------------------------+
#' 
#' @param model (Model) Set error model for this model
#' @param data_trans (str or expression) A data transformation expression or NULL (default) to use the transformation
#'  specified by the model.
#' @param zero_protection (logical) Set to TRUE to add code protecting from IPRED=0
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- remove_error_model(load_example_model("pheno"))
#' set_proportional_error_model(model)
#' model$statements$after_odes
#' model <- remove_error_model(load_example_model("pheno"))
#' set_proportional_error_model(
#'     model,
#'     data_trans="log(Y)"
#' model$statements$after_odes
#' }
#' @seealso
#' set_additive_error_model : Additive error model
#' 
#' set_combined_error_model : Combined error model
#' 
#' 
#' @export
set_proportional_error_model <- function(model, data_trans=NULL, zero_protection=TRUE) {
	func_out <- pharmpy$modeling$set_proportional_error_model(model, data_trans=data_trans, zero_protection=zero_protection)
	return(py_to_r(func_out))
}

#' @title
#' set_seq_zo_fo_absorption
#' 
#' @description
#' Set or change to sequential zero order first order absorption rate.
#' 
#' Initial estimate for
#' absorption rate is set the previous rate if available, otherwise it is set to the time of
#' first observation/2.
#' 
#' Currently lagtime together with sequential zero order first order absorption is not
#' supported.
#' 
#' @param model (Model) Model to set or change absorption rate
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_seq_zo_fo_absorption(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_bolus_order_absorption
#' 
#' set_zero_order_absorption
#' 
#' set_first_order_absorption
#' 
#' 
#' @export
set_seq_zo_fo_absorption <- function(model) {
	func_out <- pharmpy$modeling$set_seq_zo_fo_absorption(model)
	return(py_to_r(func_out))
}

#' @title
#' set_time_varying_error_model
#' 
#' @description
#' Set a time varying error model per time cutoff
#' 
#' @param model (Model) Pharmpy model
#' @param cutoff (numeric) A value at the given quantile over idv column
#' @param idv (str) Time or time after dose, default is Time
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_time_varying_error_model(model, cutoff=1.0)
#' model$statements$find_assignment("Y")
#' }
#' 
#' @export
set_time_varying_error_model <- function(model, cutoff, idv='TIME') {
	func_out <- pharmpy$modeling$set_time_varying_error_model(model, cutoff, idv=idv)
	return(py_to_r(func_out))
}

#' @title
#' set_transit_compartments
#' 
#' @description
#' Set the number of transit compartments of model.
#' 
#' Initial estimate for absorption rate is
#' set the previous rate if available, otherwise it is set to the time of first observation/2.
#' 
#' @param model (Model) Pharmpy model
#' @param n (integer) Number of transit compartments
#' @param keep_depot (logical) FALSE to convert depot compartment into a transit compartment
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_transit_compartments(model, 3)
#' model$statements$ode_system
#' }
#' @seealso
#' add_lag_time
#' 
#' 
#' @export
set_transit_compartments <- function(model, n, keep_depot=TRUE) {
	func_out <- pharmpy$modeling$set_transit_compartments(model, n, keep_depot=keep_depot)
	return(py_to_r(func_out))
}

#' @title
#' set_upper_bounds
#' 
#' @description
#' Set parameter upper bounds
#' 
#' @param model (Model) Pharmpy model
#' @param bounds (list) A list of parameter bounds for parameters to change
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_upper_bounds(model, list('THETA(1)'=10))
#' model$parameters['THETA(1)']
#' }
#' @seealso
#' set_lower_bounds : Set parameter lower bounds
#' 
#' unconstrain_parameters : Remove all constraints of parameters
#' 
#' 
#' @export
set_upper_bounds <- function(model, bounds) {
	func_out <- pharmpy$modeling$set_upper_bounds(model, bounds)
	return(py_to_r(func_out))
}

#' @title
#' set_weighted_error_model
#' 
#' @description
#' Encode error model with one epsilon and W as weight
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_weighted_error_model(model)
#' }
#' @seealso
#' use_thetas_for_error_stdev : Use thetas to estimate error
#' 
#' 
#' @export
set_weighted_error_model <- function(model) {
	func_out <- pharmpy$modeling$set_weighted_error_model(model)
	return(py_to_r(func_out))
}

#' @title
#' set_zero_order_absorption
#' 
#' @description
#' Set or change to zero order absorption rate.
#' 
#' Initial estimate for absorption rate is set
#' the previous rate if available, otherwise it is set to the time of first observation/2.
#' 
#' @param model (Model) Model to set or change to first order absorption rate
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_zero_order_absorption(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_bolus_order_absorption
#' 
#' set_first_order_absorption
#' 
#' 
#' @export
set_zero_order_absorption <- function(model) {
	func_out <- pharmpy$modeling$set_zero_order_absorption(model)
	return(py_to_r(func_out))
}

#' @title
#' set_zero_order_elimination
#' 
#' @description
#' Sets elimination to zero order.
#' 
#' Initial estimate for KM is set to 1% of smallest observation.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' set_zero_order_elimination(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_first_order_elimination
#' 
#' set_michaelis_menten_elimination
#' 
#' 
#' @export
set_zero_order_elimination <- function(model) {
	func_out <- pharmpy$modeling$set_zero_order_elimination(model)
	return(py_to_r(func_out))
}

#' @title
#' simplify_expression
#' 
#' @description
#' Simplify expression given constraints in model
#' 
#' @param model (Model) Pharmpy model object
#' @param expr (Expression) Expression to simplify
#'  
#' @return (Expression) Simplified expression
#' 
#' @examples
#' \dontrun{
#' conf$parameter_names <- c('comment', 'basic')
#' model <- load_example_model("pheno")
#' simplify_expression(model, "Abs(PTVCL)")
#' conf$parameter_names <- c('basic')
#' }
#' 
#' @export
simplify_expression <- function(model, expr) {
	func_out <- pharmpy$modeling$simplify_expression(model, expr)
	return(py_to_r(func_out))
}

#' @title
#' solve_ode_system
#' 
#' @description
#' Replace ODE system with analytical solution if possible
#' 
#' Warnings
#' This function can currently only handle the most simple of ODE systems.
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Model) Reference to the same pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements$ode_system
#' solve_ode_system(model)
#' }
#' 
#' @export
solve_ode_system <- function(model) {
	func_out <- pharmpy$modeling$solve_ode_system(model)
	return(py_to_r(func_out))
}

#' @title
#' split_joint_distribution
#' 
#' @description
#' Splits etas following a joint distribution into separate distributions.
#' 
#' @param model (Model) Pharmpy model
#' @param rvs (str, vector) Name/names of etas to separate. If NULL, all etas that are IIVs and
#'  non-fixed will become single. NULL is default.
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' create_joint_distribution(model, c('ETA(1)', 'ETA(2)'))
#' model$random_variables$etas
#' split_joint_distribution(model, c('ETA(1)', 'ETA(2)'))
#' model$random_variables$etas
#' }
#' @seealso
#' create_joint_distribution : combine etas into a join distribution
#' 
#' 
#' @export
split_joint_distribution <- function(model, rvs=NULL) {
	func_out <- pharmpy$modeling$split_joint_distribution(model, rvs=rvs)
	return(py_to_r(func_out))
}

#' @title
#' transform_etas_boxcox
#' 
#' @description
#' Applies a boxcox transformation to selected etas
#' 
#' Initial estimate for lambda is 0.1 with bounds (-3, 3).
#' 
#' @param model (Model) Pharmpy model to apply boxcox transformation to.
#' @param list_of_etas (str, vector) Name/names of etas to transform. If NULL, all etas will be transformed (default).
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' transform_etas_boxcox(model, c("ETA(1)"))
#' model$statements$before_odes$full_expression("CL")
#' }
#' @seealso
#' transform_etas_tdist
#' 
#' transform_etas_john_draper
#' 
#' 
#' @export
transform_etas_boxcox <- function(model, list_of_etas=NULL) {
	func_out <- pharmpy$modeling$transform_etas_boxcox(model, list_of_etas=list_of_etas)
	return(py_to_r(func_out))
}

#' @title
#' transform_etas_john_draper
#' 
#' @description
#' Applies a John Draper transformation (1) to spelected etas
#' 
#' Initial estimate for lambda is 0.1 with bounds (-3, 3).
#' 
#' (1) John, J., Draper, N. (1980). An Alternative Family of Transformations.
#' Journal of the Royal Statistical Society. Series C (Applied Statistics),
#' 29(2), 190-197. doi:10.2307/2986305
#' 
#' @param model (Model) Pharmpy model to apply John Draper transformation to.
#' @param list_of_etas (str, vector) Name/names of etas to transform. If NULL, all etas will be transformed (default).
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' transform_etas_john_draper(model, c("ETA(1)"))
#' model$statements$before_odes$full_expression("CL")
#' }
#' @seealso
#' transform_etas_boxcox
#' 
#' transform_etas_tdist
#' 
#' 
#' @export
transform_etas_john_draper <- function(model, list_of_etas=NULL) {
	func_out <- pharmpy$modeling$transform_etas_john_draper(model, list_of_etas=list_of_etas)
	return(py_to_r(func_out))
}

#' @title
#' transform_etas_tdist
#' 
#' @description
#' Applies a t-distribution transformation to selected etas
#' 
#' Initial estimate for degrees of freedom is 80 with bounds (3, 100).
#' 
#' @param model (Model) Pharmpy model to apply t distribution transformation to.
#' @param list_of_etas (str, vector) Name/names of etas to transform. If NULL, all etas will be transformed (default).
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' transform_etas_tdist(model, c("ETA(1)"))
#' model$statements$before_odes$full_expression("CL")
#' }
#' @seealso
#' transform_etas_boxcox
#' 
#' transform_etas_john_draper
#' 
#' 
#' @export
transform_etas_tdist <- function(model, list_of_etas=NULL) {
	func_out <- pharmpy$modeling$transform_etas_tdist(model, list_of_etas=list_of_etas)
	return(py_to_r(func_out))
}

#' @title
#' translate_nmtran_time
#' 
#' @description
#' Translate NM-TRAN TIME and DATE column into one TIME column
#' 
#' If dataset of model have special NM-TRAN TIME and DATE columns these
#' will be translated into one single time column with time in hours.
#' 
#' Warnings
#' Use this function with caution. For example reset events are currently not taken into account.
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Model) Reference to the same model object
#' 
#' 
#' @export
translate_nmtran_time <- function(model) {
	func_out <- pharmpy$modeling$translate_nmtran_time(model)
	return(py_to_r(func_out))
}

#' @title
#' unconstrain_parameters
#' 
#' @description
#' Remove all constraints from parameters
#' 
#' @param model (Model) Pharmpy model
#' @param parameter_names (vector) Remove all constraints for the listed parameters
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$parameters['THETA(1)']
#' unconstrain_parameters(model, c('THETA(1)'))
#' model$parameters['THETA(1)']
#' }
#' @seealso
#' set_lower_bounds : Set parameter lower bounds
#' 
#' set_upper_bounds : Set parameter upper bounds
#' 
#' unfix_parameters : Unfix parameters
#' 
#' 
#' @export
unconstrain_parameters <- function(model, parameter_names) {
	func_out <- pharmpy$modeling$unconstrain_parameters(model, parameter_names)
	return(py_to_r(func_out))
}

#' @title
#' undrop_columns
#' 
#' @description
#' Undrop columns of model
#' 
#' @param model (Model) Pharmpy model object
#' @param column_names (vector or str) List of column names or one column name to undrop
#'  
#' @return (Model) Reference to same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' drop_columns(model, c('WGT', 'APGR'), mark=TRUE)
#' undrop_columns(model, 'WGT')
#' }
#' @seealso
#' drop_dropped_columns : Drop all columns marked as drop
#' 
#' drop_columns : Drop or mark columns as dropped
#' 
#' 
#' @export
undrop_columns <- function(model, column_names) {
	func_out <- pharmpy$modeling$undrop_columns(model, column_names)
	return(py_to_r(func_out))
}

#' @title
#' unfix_parameters
#' 
#' @description
#' Unfix parameters
#' 
#' Unfix all listed parameters
#' 
#' @param model (Model) Pharmpy model
#' @param parameter_names (vector or str) one parameter name or a vector of parameter names
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' fix_parameters(model, c('THETA(1)', 'THETA(2)', 'THETA(3)'))
#' model$parameters$fix
#' unfix_parameters(model, 'THETA(1)')
#' model$parameters$fix
#' }
#' @seealso
#' unfix_paramaters_to : Unfixing parameters and setting a new initial estimate in the same
#' 
#' function
#' 
#' fix_parameters : Fix parameters
#' 
#' fix_or_unfix_parameters : Fix or unfix parameters (given boolean)
#' 
#' fix_parameters_to : Fixing and setting parameter initial estimates in the same function
#' 
#' unconstrain_parameters : Remove all constraints of parameters
#' 
#' 
#' @export
unfix_parameters <- function(model, parameter_names) {
	func_out <- pharmpy$modeling$unfix_parameters(model, parameter_names)
	return(py_to_r(func_out))
}

#' @title
#' unfix_parameters_to
#' 
#' @description
#' Unfix parameters to
#' 
#' Unfix all listed parameters to specified value/values
#' 
#' @param model (Model) Pharmpy model
#' @param inits (list) Inits for all parameters to unfix and change init
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' fix_parameters(model, c('THETA(1)', 'THETA(2)', 'THETA(3)'))
#' model$parameters$fix
#' unfix_parameters_to(model, {'THETA(1)': 0.5})
#' model$parameters$fix
#' model$parameters['THETA(1)']
#' }
#' @seealso
#' fix_parameters : Fix parameters
#' 
#' fix_or_unfix_parameters : Fix or unfix parameters (given boolean)
#' 
#' unfix_paramaters : Unfixing parameters
#' 
#' fix_paramaters_to : Fixing parameters and setting a new initial estimate in the same
#' 
#' function
#' 
#' 
#' @export
unfix_parameters_to <- function(model, inits) {
	func_out <- pharmpy$modeling$unfix_parameters_to(model, inits)
	return(py_to_r(func_out))
}

#' @title
#' update_initial_individual_estimates
#' 
#' @description
#' Update initial individual estimates for a model
#' 
#' Updates initial individual estimates for a model.
#' 
#' @param model (Model) Pharmpy model to update initial estimates
#' @param individual_estimates (data.frame) Individual estimates to use
#' @param force (logical) Set to FALSE to only update if the model had initial individual estimates before
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' ie <- model$modelfit_results$individual_estimates
#' model <- update_initial_individual_estimates(model, ie)
#' }
#' 
#' @export
update_initial_individual_estimates <- function(model, individual_estimates, force=TRUE) {
	func_out <- pharmpy$modeling$update_initial_individual_estimates(model, individual_estimates, force=force)
	return(py_to_r(func_out))
}

#' @title
#' update_inits
#' 
#' @description
#' Update initial parameter estimate for a model
#' 
#' Updates initial estimates of population parameters for a model.
#' If the new initial estimates are out of bounds or NaN this function will raise.
#' 
#' @param model (Model) Pharmpy model to update initial estimates
#' @param parameter_estimates (data.frame) Parameter estimates to update
#' @param move_est_close_to_bounds (logical) Move estimates that are close to bounds. If correlation >0.99 the correlation will
#'  be set to 0.9, if variance is <0.001 the variance will be set to 0.01.
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")   # This model was previously fitted to its data
#' model$parameters$inits
#' update_inits(model, model$modelfit_results$parameter_estimates)
#' model$parameters$inits
#' }
#' 
#' @export
update_inits <- function(model, parameter_estimates, move_est_close_to_bounds=FALSE) {
	func_out <- pharmpy$modeling$update_inits(model, parameter_estimates, move_est_close_to_bounds=move_est_close_to_bounds)
	return(py_to_r(func_out))
}

#' @title
#' use_thetas_for_error_stdev
#' 
#' @description
#' Use thetas to estimate standard deviation of error
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' use_thetas_for_error_stdev(model)
#' model$statements$find_assignment("Y")
#' }
#' @seealso
#' set_weighted_error_model : Encode error model with one epsilon and weight
#' 
#' 
#' @export
use_thetas_for_error_stdev <- function(model) {
	func_out <- pharmpy$modeling$use_thetas_for_error_stdev(model)
	return(py_to_r(func_out))
}

#' @title
#' write_csv
#' 
#' @description
#' Write dataset to a csv file and updates the datainfo path
#' 
#' @param model (Model) Model whose dataset to write to file
#' @param path (NULL or str or Path) Destination path. Default is to use original path with .csv suffix.
#' @param force (logical) Overwrite file with same path. Default is FALSE.
#'  
#' @return (Path) path to the written file.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' write_csv(model, path="newdataset$csv")
#' }
#' 
#' @export
write_csv <- function(model, path=NULL, force=FALSE) {
	func_out <- pharmpy$modeling$write_csv(model, path=path, force=force)
	return(py_to_r(func_out))
}

#' @title
#' write_model
#' 
#' @description
#' Write model code to file
#' 
#' @param model (Model) Pharmpy model
#' @param path (str) Destination path
#' @param force (logical) Force overwrite, default is TRUE
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' write_model(model)
#' }
#' 
#' @export
write_model <- function(model, path='', force=TRUE) {
	func_out <- pharmpy$modeling$write_model(model, path=path, force=force)
	return(py_to_r(func_out))
}

#' @title
#' create_results
#' 
#' @description
#' Create/recalculate results object given path to run directory
#' 
#' @param path (str, Path) Path to run directory
#' @param ... Arguments to pass to tool specific create results function
#'  
#' @return (Results) Results object for tool
#' 
#' @examples
#' \dontrun{
#' res <- create_results("frem_dir1")
#' }
#' @seealso
#' read_results
#' 
#' 
#' @export
create_results <- function(path, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$create_results(path, ...)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' fit
#' 
#' @description
#' Fit models.
#' 
#' @param model_or_models (Model | vector of Model) List of models or one single model
#' @param tool (str) Estimation tool to use. NULL to use default
#'  
#' @return (ModelfitResults | vector of ModelfitResults) ModelfitResults for the model or models
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- fit(model)
#' }
#' @seealso
#' run_tool
#' 
#' 
#' @export
fit <- function(model_or_models, tool=NULL) {
	tryCatch(
	{
		func_out <- pharmpy$tools$fit(model_or_models, tool=tool)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' predict_influential_individuals
#' 
#' @description
#' Predict influential individuals for a model using a machine learning model.
#' 
#' @param model (Model) Pharmpy model
#' @param results (ModelfitResults) Results for model
#' @param cutoff (numeric) Cutoff threshold for a dofv signalling an influential individual
#'  
#' @return (data.frame) Dataframe over the individuals with a `dofv` column containing the raw predicted delta-OFV and an `influential` column with a boolean to tell whether the individual is influential or not.
#' 
#' @seealso
#' predict_influential_outliers
#' 
#' predict_outliers
#' 
#' 
#' @export
predict_influential_individuals <- function(model, results, cutoff=3.84) {
	tryCatch(
	{
		func_out <- pharmpy$tools$predict_influential_individuals(model, results, cutoff=cutoff)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' predict_influential_outliers
#' 
#' @description
#' Predict influential outliers for a model using a machine learning model.
#' 
#' @param model (Model) Pharmpy model
#' @param results (ModelfitResults) Results for model
#' @param outlier_cutoff (numeric) Cutoff threshold for a residual singalling an outlier
#' @param influential_cutoff (numeric) Cutoff threshold for a dofv signalling an influential individual
#'  
#' @return (data.frame) Dataframe over the individuals with a `outliers` and `dofv` columns containing the raw predictions and `influential`, `outlier` and `influential_outlier` boolean columns.
#' 
#' @seealso
#' predict_influential_individuals
#' 
#' predict_outliers
#' 
#' 
#' @export
predict_influential_outliers <- function(model, results, outlier_cutoff=3, influential_cutoff=3.84) {
	tryCatch(
	{
		func_out <- pharmpy$tools$predict_influential_outliers(model, results, outlier_cutoff=outlier_cutoff, influential_cutoff=influential_cutoff)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' predict_outliers
#' 
#' @description
#' Predict outliers for a model using a machine learning model.
#' 
#' See the :ref:`simeval <Individual OFV summary>` documentation for a definition of the `residual`
#' 
#' @param model (Model) Pharmpy model
#' @param results (ModelfitResults) ModelfitResults for the model
#' @param cutoff (numeric) Cutoff threshold for a residual singalling an outlier
#'  
#' @return (data.frame) Dataframe over the individuals with a `residual` column containing the raw predicted residuals and a `outlier` column with a boolean to tell whether the individual is an outlier or not.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- model$modelfit_results
#' predict_outliers(model, results)
#' }
#' @seealso
#' predict_influential_individuals
#' 
#' predict_influential_outliers
#' 
#' 
#' @export
predict_outliers <- function(model, results, cutoff=3.0) {
	tryCatch(
	{
		func_out <- pharmpy$tools$predict_outliers(model, results, cutoff=cutoff)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' print_fit_summary
#' 
#' @description
#' Print a summary of the model fit
#' 
#' @param model (Model) Pharmpy model object
#' 
#' @export
print_fit_summary <- function(model) {
	tryCatch(
	{
		func_out <- pharmpy$tools$print_fit_summary(model)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' rank_models
#' 
#' @description
#' Ranks a vector of models
#' 
#' Ranks a vector of models with a given ranking function
#' 
#' @param base_model (Model) Base model to compare to
#' @param models (vector) List of models
#' @param errors_allowed (vector or NULL) List of errors that are allowed for ranking. Currently available is: rounding_errors and
#'  maxevals_exceeded. Default is NULL
#' @param rank_type (str) Name of ranking type. Available options are 'ofv', 'aic', 'bic', 'lrt' (OFV with LRT)
#' @param cutoff (numeric or NULL) Value to use as cutoff. If using LRT, cutoff denotes p-value. Default is NULL
#' @param bic_type (str) Type of BIC to calculate. Default is the mixed effects.
#'  
#' @return (data.frame) DataFrame of the ranked models
#' 
#' @examples
#' \dontrun{
#' model_1 <- load_example_model("pheno")
#' model_2 <- load_example_model("pheno_linear")
#' rank_models(model_1, c(model_2),
#'             errors_allowed=c('rounding_errors'),
#'             rank_type='lrt')
#' }
#' 
#' @export
rank_models <- function(base_model, models, errors_allowed=NULL, rank_type='ofv', cutoff=NULL, bic_type='mixed') {
	tryCatch(
	{
		func_out <- pharmpy$tools$rank_models(base_model, models, errors_allowed=errors_allowed, rank_type=rank_type, cutoff=cutoff, bic_type=bic_type)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' read_modelfit_results
#' 
#' @description
#' Read results from external tool for a model
#' 
#' @param path (Path or str) Path to model file
#'  
#' @return (ModelfitResults) Results object
#' 
#' 
#' @export
read_modelfit_results <- function(path) {
	tryCatch(
	{
		func_out <- pharmpy$tools$read_modelfit_results(path)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' read_results
#' 
#' @description
#' Read results object from file
#' 
#' @param path (str, Path) Path to results file
#'  
#' @return (Results) Results object for tool
#' 
#' @examples
#' \dontrun{
#' res <- read_results("results$json")
#' }
#' @seealso
#' create_results
#' 
#' 
#' @export
read_results <- function(path) {
	tryCatch(
	{
		func_out <- pharmpy$tools$read_results(path)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' retrieve_final_model
#' 
#' @description
#' Retrieve final model from a result object
#' 
#' @param res (Results) A results object
#'  
#' @return (Model) Reference to final model
#' 
#' @examples
#' \dontrun{
#' res <- read_results("results$json")
#' model <- retrieve_final_model(res)
#' }
#' @seealso
#' retrieve_models
#' 
#' 
#' @export
retrieve_final_model <- function(res) {
	tryCatch(
	{
		func_out <- pharmpy$tools$retrieve_final_model(res)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' retrieve_models
#' 
#' @description
#' Retrieve models after a tool run
#' 
#' Any models created and run by the tool can be
#' retrieved.
#' 
#' @param source (str, Path, Results, ToolDatabase, ModelDatabase) Source where to find models. Can be a path (as str or Path), a results object, or a
#'  ToolDatabase/ModelDatabase
#' @param names (vector) List of names of the models to retrieve or NULL for all
#'  
#' @return (vector) List of retrieved model objects
#' 
#' @examples
#' \dontrun{
#' tooldir_path <- 'path/to/tool/directory'
#' models <- retrieve_models(tooldir_path, names=c('run1'))
#' }
#' @seealso
#' retrieve_final_model
#' 
#' 
#' @export
retrieve_models <- function(source, names=NULL) {
	tryCatch(
	{
		func_out <- pharmpy$tools$retrieve_models(source, names=names)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' run_allometry
#' 
#' @description
#' Run allometry tool. For more details, see :ref:`allometry`.
#' 
#' @param model (Model) Pharmpy model
#' @param results (ModelfitResults) Results for model
#' @param allometric_variable (str) Name of the variable to use for allometric scaling (default is WT)
#' @param reference_value (numeric) Reference value for the allometric variable (default is 70)
#' @param parameters (vector) Parameters to apply scaling to (default is all CL, Q and V parameters)
#' @param initials (vector) Initial estimates for the exponents. (default is to use 0.75 for CL and Qs and 1 for Vs)
#' @param lower_bounds (vector) Lower bounds for the exponents. (default is 0 for all parameters)
#' @param upper_bounds (vector) Upper bounds for the exponents. (default is 2 for all parameters)
#' @param fixed (logical) Should the exponents be fixed or not. (default TRUE
#' @param ... Arguments to pass to tool
#'  
#' @return (AllometryResults) Allometry tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' run_allometry(model=model, results=model$modelfit_results, allometric_variable='WGT')
#' }
#' 
#' @export
run_allometry <- function(model=NULL, results=NULL, allometric_variable='WT', reference_value=70, parameters=NULL, initials=NULL, lower_bounds=NULL, upper_bounds=NULL, fixed=TRUE, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_allometry(model=model, results=results, allometric_variable=allometric_variable, reference_value=reference_value, parameters=parameters, initials=initials, lower_bounds=lower_bounds, upper_bounds=upper_bounds, fixed=fixed, ...)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' run_amd
#' 
#' @description
#' Run Automatic Model Development (AMD) tool
#' 
#' Runs structural modelsearch, IIV building, and ruvsearch
#' 
#' @param input (Model or Path) Read model object/Path to a dataset
#' @param results (ModelfitResults) Reults of input if input is a model
#' @param modeltype (str) Type of model to build. Either 'pk_oral' or 'pk_iv'
#' @param cl_init (numeric) Initial estimate for the population clearance
#' @param vc_init (numeric) Initial estimate for the central compartment population volume
#' @param mat_init (numeric) Initial estimate for the mean absorption time (not for iv models)
#' @param search_space (str) MFL for search space for structural model
#' @param lloq (numeric) Lower limit of quantification. LOQ data will be removed.
#' @param order (vector) Runorder of components
#' @param categorical (vector) List of categorical covariates
#' @param continuous (vector) List of continuous covariates
#' @param allometric_variable (str or Symbol) Variable to use for allometry
#' @param occasion (str) Name of occasion column
#' @param path (str or Path) Path to run AMD in
#' @param resume (logical) Whether to allow resuming previous run
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' run_amd(model, results=model$modelfit_results)
#' }
#' @seealso
#' run_iiv
#' 
#' run_tool
#' 
#' 
#' @export
run_amd <- function(input, results=NULL, modeltype='pk_oral', cl_init=0.01, vc_init=1, mat_init=0.1, search_space=NULL, lloq=NULL, order=NULL, categorical=NULL, continuous=NULL, allometric_variable=NULL, occasion=NULL, path=NULL, resume=FALSE) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_amd(input, results=results, modeltype=modeltype, cl_init=cl_init, vc_init=vc_init, mat_init=mat_init, search_space=search_space, lloq=lloq, order=order, categorical=categorical, continuous=continuous, allometric_variable=allometric_variable, occasion=occasion, path=path, resume=resume)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' run_covsearch
#' 
#' @description
#' Run COVsearch tool. For more details, see :ref:`covsearch`.
#' 
#' @param effects (str | vector) The vector of candidate parameter-covariate effects to try, either as a
#'  MFL sentence or in (optionally compact) tuple form.
#' @param p_forward (numeric) The p-value to use in the likelihood ratio test for forward steps
#' @param p_backward (numeric) The p-value to use in the likelihood ratio test for backward steps
#' @param max_steps (integer) The maximum number of search steps to make
#' @param algorithm (str) The search algorithm to use. Currently 'scm-forward' and
#'  'scm-forward-then-backward' are supported.
#' @param results (ModelfitResults) Results of model
#' @param model (Model) Pharmpy mode
#' @param ... Arguments to pass to tool
#'  
#' @return (COVSearchResults) COVsearch tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' res <- run_covsearch([
#' }
#' 
#' @export
run_covsearch <- function(effects, p_forward=0.05, p_backward=0.01, max_steps=-1, algorithm='scm-forward-then-backward', results=NULL, model=NULL, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_covsearch(effects, p_forward=p_forward, p_backward=p_backward, max_steps=max_steps, algorithm=algorithm, results=results, model=model, ...)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' run_iivsearch
#' 
#' @description
#' Run IIVsearch tool. For more details, see :ref:`iivsearch`.
#' 
#' @param algorithm (str) Which algorithm to run (brute_force, brute_force_no_of_etas, brute_force_block_structure)
#' @param iiv_strategy (str) If/how IIV should be added to start model. Possible strategies are 'no_add', 'add_diagonal',
#'  or 'fullblock'. Default is 'no_add'
#' @param rank_type (str) Which ranking type should be used (OFV, AIC, BIC). Default is BIC
#' @param cutoff (numeric) Cutoff for which value of the ranking function that is considered significant. Default
#'  is NULL (all models will be ranked)
#' @param results (ModelfitResults) Results for model
#' @param model (Model) Pharmpy mode
#' @param ... Arguments to pass to tool
#'  
#' @return (IIVSearchResults) IIVsearch tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' run_iivsearch('brute_force', results=model$modelfit_results, model=model)
#' }
#' 
#' @export
run_iivsearch <- function(algorithm, iiv_strategy='no_add', rank_type='bic', cutoff=NULL, results=NULL, model=NULL, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_iivsearch(algorithm, iiv_strategy=iiv_strategy, rank_type=rank_type, cutoff=cutoff, results=results, model=model, ...)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' run_iovsearch
#' 
#' @description
#' Run IOVsearch tool. For more details, see :ref:`iovsearch`.
#' 
#' @param column (str) Name of column in dataset to use as occasion column (default is 'OCC')
#' @param list_of_parameters (NULL or vector) List of parameters to test IOV on, if none all parameters with IIV will be tested (default)
#' @param rank_type (str) Which ranking type should be used (OFV, AIC, BIC). Default is BIC
#' @param cutoff (NULL or numeric) Cutoff for which value of the ranking type that is considered significant. Default
#'  is NULL (all models will be ranked)
#' @param distribution (str) Which distribution added IOVs should have (default is same-as-iiv)
#' @param results (ModelfitResults) Results for model
#' @param model (Model) Pharmpy mode
#' @param ... Arguments to pass to tool
#'  
#' @return (IOVSearchResults) IOVSearch tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' run_iovsearch('OCC', results=model$modelfit_results, model=model)
#' }
#' 
#' @export
run_iovsearch <- function(column='OCC', list_of_parameters=NULL, rank_type='bic', cutoff=NULL, distribution='same-as-iiv', results=NULL, model=NULL, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_iovsearch(column=column, list_of_parameters=list_of_parameters, rank_type=rank_type, cutoff=cutoff, distribution=distribution, results=results, model=model, ...)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' run_modelfit
#' 
#' @description
#' Run modelfit tool.
#' 
#' @param models (Model) A vector of models are one single model object
#' @param n (integer) Number of models to fit. This is only used if the tool is going to be combined with other tools.
#' @param tool (str) Which tool to use for fitting. Currently 'nonmem' or 'nlmixr' can be used
#' @param ... Arguments to pass to tool
#'  
#' @return (ModelfitResults) Modelfit tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' run_modelfit(model)
#' }
#' 
#' @export
run_modelfit <- function(models=NULL, n=NULL, tool=NULL, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_modelfit(models=models, n=n, tool=tool, ...)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' run_modelsearch
#' 
#' @description
#' Run Modelsearch tool. For more details, see :ref:`modelsearch`.
#' 
#' @param search_space (str) Search space to test
#' @param algorithm (str) Algorithm to use (e.g. exhaustive)
#' @param iiv_strategy (str) If/how IIV should be added to candidate models. Possible strategies are 'no_add',
#'  'add_diagonal', 'fullblock', or 'absorption_delay'. Default is 'absorption_delay'
#' @param rank_type (str) Which ranking type should be used (OFV, AIC, BIC). Default is BIC
#' @param cutoff (numeric) Cutoff for which value of the ranking function that is considered significant. Default
#'  is NULL (all models will be ranked)
#' @param results (ModelfitResults) Results for model
#' @param model (Model) Pharmpy mode
#' @param ... Arguments to pass to tool
#'  
#' @return (ModelSearchResults) Modelsearch tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' res <- model$modelfit_results
#' run_modelsearch('ABSORPTION(ZO);PERIPHERALS(1)', 'exhaustive', results=res, model=model)
#' }
#' 
#' @export
run_modelsearch <- function(search_space, algorithm, iiv_strategy='absorption_delay', rank_type='bic', cutoff=NULL, results=NULL, model=NULL, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_modelsearch(search_space, algorithm, iiv_strategy=iiv_strategy, rank_type=rank_type, cutoff=cutoff, results=results, model=model, ...)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' run_ruvsearch
#' 
#' @description
#' Run the ruvsearch tool. For more details, see :ref:`ruvsearch`.
#' 
#' @param model (Model) Pharmpy model
#' @param results (ModelfitResults) Results of model
#' @param groups (integer) The number of bins to use for the time varying models
#' @param p_value (numeric) The p-value to use for the likelihood ratio test
#' @param skip (vector) A vector of models to not attempt
#' @param ... Arguments to pass to tool
#'  
#' @return (RUVSearchResults) Ruvsearch tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' run_ruvsearch(model=model, results=model$modelfit_results)
#' }
#' 
#' @export
run_ruvsearch <- function(model=NULL, results=NULL, groups=4, p_value=0.05, skip=NULL, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_ruvsearch(model=model, results=results, groups=groups, p_value=p_value, skip=skip, ...)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' run_tool
#' 
#' @description
#' Run tool workflow
#' 
#' @param name (str) Name of tool to run
#' @param ... Arguments to pass to tool
#'  
#' @return (Results) Results object for tool
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' res <- run_tool("ruvsearch", model)
#' }
#' 
#' @export
run_tool <- function(name, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_tool(name, ...)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' summarize_errors
#' 
#' @description
#' Summarize errors and warnings from one or multiple model runs.
#' 
#' Summarize the errors and warnings found after running the model/models.
#' 
#' @param models (vector, Model) List of models or single model
#'  
#' @return (data.frame) A DataFrame of errors with model name, category (error or warning), and an integer as index, an empty DataFrame if there were no errors or warnings found.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' summarize_errors(model)
#' }
#' 
#' @export
summarize_errors <- function(models) {
	tryCatch(
	{
		func_out <- pharmpy$tools$summarize_errors(models)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' summarize_individuals
#' 
#' @description
#' Creates a summary dataframe keyed by model-individual pairs for an input
#' vector of models.
#' 
#' Content of the various columns:
#' 
#' +-------------------------+----------------------------------------------------------------------+
#' | Column                  | Description                                                          |
#' +=========================+======================================================================+
#' | ``outlier_count``       | Number of observations with CWRES > 5                                |
#' +-------------------------+----------------------------------------------------------------------+
#' | ``ofv``                 | Individual OFV                                                       |
#' +-------------------------+----------------------------------------------------------------------+
#' | ``dofv_vs_parent``      | Difference in individual OFV between this model and its parent model |
#' +-------------------------+----------------------------------------------------------------------+
#' | ``predicted_dofv``      | Predicted dOFV if this individual was excluded                       |
#' +-------------------------+----------------------------------------------------------------------+
#' | ``predicted_residual``  | Predicted residual                                                   |
#' +-------------------------+----------------------------------------------------------------------+
#' 
#' @param models (vector of Model) Input models
#'  
#' @return (data.frame | NULL) The summary as a dataframe
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' fit_results <- fit(model)
#' results <- run_tool(
#'     model=model,
#'     mfl='ABSORPTION(ZO);PERIPHERALS(c(1, 2))',
#'     algorithm='reduced_stepwise'
#' summarize_individuals([results$start_model, *results$models])
#' }
#' 
#' @export
summarize_individuals <- function(models) {
	tryCatch(
	{
		func_out <- pharmpy$tools$summarize_individuals(models)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' summarize_individuals_count_table
#' 
#' @description
#' Create a count table for individual data
#' 
#' Content of the various columns:
#' 
#' +-------------------------+------------------------------------------------------------------------------------------------+
#' | Column                  | Description                                                                                    |
#' +=========================+================================================================================================+
#' | ``inf_selection``       | Number of subjects influential on model selection.                                             |
#' |                         | :math:`\mathrm{OFV}_{parent} - \mathrm{OFV} > 3.84 \veebar`                                    |
#' |                         | :math:`\mathrm{OFV}_{parent} - \mathrm{iOFV}_{parent} - (\mathrm{OFV} - \mathrm{iOFV}) > 3.84` |
#' +-------------------------+------------------------------------------------------------------------------------------------+
#' | ``inf_params``          | Number of subjects influential on parameters. predicted_dofv > 3.84                            |
#' +-------------------------+------------------------------------------------------------------------------------------------+
#' | ``out_obs``             | Number of subjects having at least one outlying observation (CWRES > 5)                        |
#' +-------------------------+------------------------------------------------------------------------------------------------+
#' | ``out_ind``             | Number of outlying subjects. predicted_residual > 3.0                                          |
#' +-------------------------+------------------------------------------------------------------------------------------------+
#' | ``inf_outlier``         | Number of subjects both influential by any criteria and outlier by any criteria                |
#' +-------------------------+------------------------------------------------------------------------------------------------+
#' 
#' @param models (vector of models) List of models to summarize.
#' @param df (data.frame) Output from a previous call to summarize_individuals.
#'  
#' @return (data.frame) Table with one row per model.
#' 
#' @seealso
#' summarize_individuals : Get raw individual data
#' 
#' 
#' @export
summarize_individuals_count_table <- function(models=NULL, df=NULL) {
	tryCatch(
	{
		func_out <- pharmpy$tools$summarize_individuals_count_table(models=models, df=df)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' summarize_modelfit_results
#' 
#' @description
#' Summarize results of model runs
#' 
#' Summarize different results after fitting a model, includes runtime, ofv,
#' and parameter estimates (with errors). If include_all_estimation_steps is FALSE,
#' only the last estimation step will be included (note that in that case, the
#' minimization_successful value will be referring to the last estimation step, if
#' last step is evaluation it will go backwards until it finds an estimation step
#' that wasn't an evaluation).
#' 
#' @param results (vector, ModelfitResults) List of ModelfitResults or single ModelfitResults
#' @param include_all_estimation_steps (logical) Whether to include all estimation steps, default is FALSE
#'  
#' @return (data.frame) A DataFrame of modelfit results with model name and estmation step as index.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' summarize_modelfit_results(model$modelfit_results)
#' }
#' 
#' @export
summarize_modelfit_results <- function(results, include_all_estimation_steps=FALSE) {
	tryCatch(
	{
		func_out <- pharmpy$tools$summarize_modelfit_results(results, include_all_estimation_steps=include_all_estimation_steps)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

#' @title
#' write_results
#' 
#' @description
#' Write results object to json (or csv) file
#' 
#' Note that the csv-file cannot be read into a results object again.
#' 
#' @param results (Results) Pharmpy results object
#' @param path (Path) Path to results file
#' @param lzma (logical) TRUE for lzma compression. Not applicable to csv file
#' @param csv (logical) Save as csv file
#' 
#' @export
write_results <- function(results, path, lzma=FALSE, csv=FALSE) {
	tryCatch(
	{
		func_out <- pharmpy$tools$write_results(results, path, lzma=lzma, csv=csv)
		return(py_to_r(func_out))
	},
	error=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	},
	warning=function(cond) {
		message(cond)
		message('Full stack:')
		message(reticulate::py_last_error())
		message("pharmr version: ", packageVersion("pharmr"))
		message("Pharmpy version: ", print_pharmpy_version())
		return(NA)
	}
	)
}

