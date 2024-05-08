#' @title
#' add_admid
#' 
#' @description
#' Add an admid column to the model dataset and datainfo. Dependent on the
#' presence of a CMT column in order to add admid correctly.
#' 
#' When generated, admids of events in between doses is set to the last used
#' admid.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (model : Model) Pharmpy model
#' 
#' @seealso
#' get_admid : Get or create an admid column
#' 
#' get_cmt : Get or create a cmt column
#' 
#' 
#' @export
add_admid <- function(model) {
	func_out <- pharmpy$modeling$add_admid(model)
	return(py_to_r(func_out))
}

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
#' If there already exists a covariate effect (or allometric scaling) on a parameter
#' with the specified allometric variable, nothing will be added.
#' 
#' If no allometric variable is specified, it will be extracted from the dataset based on
#' the descriptor "body weight".
#' 
#' @param model (Model) Pharmpy model
#' @param allometric_variable (str or Expr (optional)) Value to use for allometry (X above)
#' @param reference_value (numeric or str or Expr) Reference value (Z above)
#' @param parameters (array(numeric or str or Expr) (optional)) Parameters to use or NULL (default) for all available CL, Q and V parameters
#' @param initials (array(numeric) (optional)) Initial estimates for the exponents. Default is to use 0.75 for CL and Qs and 1 for Vs
#' @param lower_bounds (array(numeric) (optional)) Lower bounds for the exponents. Default is 0 for all parameters
#' @param upper_bounds (array(numeric) (optional)) Upper bounds for the exponents. Default is 2 for all parameters
#' @param fixed (logical) Whether the exponents should be fixed
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- remove_covariate_effect(model, 'CL', 'WGT')
#' model <- remove_covariate_effect(model, 'V', 'WGT')
#' model <- add_allometry(model, allometric_variable='WGT')
#' model$statements$before_odes
#' }
#' 
#' @export
add_allometry <- function(model, allometric_variable=NULL, reference_value=70, parameters=NULL, initials=NULL, lower_bounds=NULL, upper_bounds=NULL, fixed=TRUE) {
	parameters <- convert_input(parameters, "list")
	initials <- convert_input(initials, "list")
	lower_bounds <- convert_input(lower_bounds, "list")
	upper_bounds <- convert_input(upper_bounds, "list")
	func_out <- pharmpy$modeling$add_allometry(model, allometric_variable=allometric_variable, reference_value=reference_value, parameters=parameters, initials=initials, lower_bounds=lower_bounds, upper_bounds=upper_bounds, fixed=fixed)
	return(py_to_r(func_out))
}

#' @title
#' add_bioavailability
#' 
#' @description
#' Add bioavailability statement for the first dose compartment of the model.
#' Can be added as a new parameter or otherwise it will be set to 1. If added as a parameter,
#' a logit transformation can also be applied.
#' 
#' @param model (Model) Pharmpy model
#' @param add_parameter (logical) Add new parameter representing bioavailability or not
#' @param logit_transform (logical) Logit transform the added bioavailability parameter.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- add_bioavailability(model)
#' }
#' @seealso
#' remove_bioavailability
#' 
#' 
#' @export
add_bioavailability <- function(model, add_parameter=TRUE, logit_transform=FALSE) {
	func_out <- pharmpy$modeling$add_bioavailability(model, add_parameter=add_parameter, logit_transform=logit_transform)
	return(py_to_r(func_out))
}

#' @title
#' add_cmt
#' 
#' @description
#' Add a CMT column to the model dataset and datainfo if not existed
#' 
#' In case of multiple doses, this method is dependent on the presence of an
#' admid column to correctly number each dose.
#' 
#' NOTE : Existing CMT is based on datainfo type being set to 'compartment'
#' and a column named 'CMT' can be replaced
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (model : Model) Pharmpy model
#' 
#' @seealso
#' get_admid : Get or create an admid column
#' 
#' get_cmt : Get or create a cmt column
#' 
#' 
#' @export
add_cmt <- function(model) {
	func_out <- pharmpy$modeling$add_cmt(model)
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
#' (equation could not be rendered, see API doc on website)
#' 
#' * Init:  0.001
#' * Upper:
#' * If median of covariate equals minimum: 100,000
#' * Otherwise: (equation could not be rendered, see API doc on website)
#' * Lower:
#' * If median of covariate equals maximum: -100,000
#' * Otherwise: (equation could not be rendered, see API doc on website)
#' * Linear function for categorical covariates (*cat*)
#' * Function:
#' * If covariate is the most common category:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * For each additional category:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Init: 0.001
#' * Upper: 5
#' * Lower: -1
#' * (alternative) Linear function for categorical covariates (*cat2*)
#' * Function:
#' * If covariate is the most common category:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * For each additional category:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Init: 0.001
#' * Upper: 6
#' * Lower: 0
#' * Piecewise linear function/"hockey-stick", continuous covariates only (*piece_lin*)
#' * Function:
#' * If cov <= median:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * If cov > median:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' 
#' * Init: 0.001
#' * Upper:
#' * For first state: (equation could not be rendered, see API doc on website)
#' * Otherwise: 100,000
#' * Lower:
#' * For first state: -100,000
#' * Otherwise: (equation could not be rendered, see API doc on website)
#' * Exponential function, continuous covariates only (*exp*)
#' * Function:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Init:
#' * If lower > 0.001 or upper < 0.001: (equation could not be rendered, see API doc on website)
#' * If estimated init is 0: (equation could not be rendered, see API doc on website)
#' * Otherwise: 0.001
#' * Upper:
#' * If min - median = 0 or max - median = 0: 100
#' * Otherwise:
#' 
#' (equation could not be rendered, see API doc on website)
#' * Lower:
#' * If min - median = 0 or max - median = 0: 0.01
#' * Otherwise:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Power function, continuous covariates only (*pow*)
#' * Function:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Init: 0.001
#' * Upper: 100,000
#' * Lower: -100
#' 
#' 
#' @param model (Model) Pharmpy model to add covariate effect to.
#' @param parameter (str) Name of parameter to add covariate effect to.
#' @param covariate (str) Name of covariate.
#' @param effect (str) Type of covariate effect. May be abbreviated covariate effect (see above) or custom.
#' @param operation (str) Whether the covariate effect should be added or multiplied (default).
#' @param allow_nested (logical) Whether to allow adding a covariate effect when one already exists for
#' the input parameter-covariate pair.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- add_covariate_effect(model, "CL", "APGR", "exp")
#' model$statements$before_odes$full_expression("CL")
#' }
#' 
#' @export
add_covariate_effect <- function(model, parameter, covariate, effect, operation='*', allow_nested=FALSE) {
	func_out <- pharmpy$modeling$add_covariate_effect(model, parameter, covariate, effect, operation=operation, allow_nested=allow_nested)
	return(py_to_r(func_out))
}

#' @title
#' add_derivative
#' 
#' @description
#' Add a derivative to be calculcated when running the model. Currently, only
#' derivatives with respect to the prediction is supported. Default is to add all possible
#' ETA and EPS derivatives.
#' First order derivates are specied either by single string or single-element tuple.
#' For instance with_respect_to = "ETA_1" or with_respect_to = ("ETA_1",)
#' 
#' Second order derivatives are specified by giving the two independent varibles in a tuple
#' of tuples. For instance with_respect_to ((ETA_1, EPS_1),)
#' 
#' Multiple derivatives can be specified within a tuple. For instance ((ETA_1, EPS_1), "ETA_1")
#' 
#' Currently, only ETAs and EPSILONs are supported
#' 
#' @param model (Model) Pharmpy modeas.
#' @param with_respect_to (array(array(str) or str) or str (optional)) Parameter name(s) to use as independent variables. Default is NULL.
#'  
#' @return (Pharmpy model.) 
#' 
#' 
#' @export
add_derivative <- function(model, with_respect_to=NULL) {
	func_out <- pharmpy$modeling$add_derivative(model, with_respect_to=with_respect_to)
	return(py_to_r(func_out))
}

#' @title
#' add_effect_compartment
#' 
#' @description
#' Add an effect compartment.
#' 
#' Implemented PD models are:
#' 
#' 
#' * Linear:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Emax:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Step effect:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Sigmoidal:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Log-linear:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' @param model (Model) Pharmpy model
#' @param expr (str) Name of the PD effect function.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- add_effect_compartment(model, "linear")
#' model$statements$ode_system$find_compartment("EFFECT")
#' }
#' 
#' @export
add_effect_compartment <- function(model, expr) {
	func_out <- pharmpy$modeling$add_effect_compartment(model, expr)
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
#' @param idx (numeric (optional)) index of estimation step (starting from 0), default is NULL (adds step at the end)
#' @param ... Arguments to pass to EstimationStep (such as interaction, evaluation)
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' opts <- list('NITER'=1000, 'ISAMPLE'=100)
#' model <- add_estimation_step(model, 'IMP', tool_options=opts)
#' ests <- model$execution_steps
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
#' add_parameter_uncertainty_step
#' 
#' remove_parameter_uncertainty_step
#' 
#' set_evaluation_step
#' 
#' 
#' @export
add_estimation_step <- function(model, method, idx=NULL, ...) {
	idx <- convert_input(idx, "int")
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
#' * Rescaled logit (*re_log*)
#' 
#' For all except exponential the operation input is not needed. Otherwise user specified
#' input is supported. Initial estimates for new etas are 0.09.
#' 
#' 
#' 
#' Assuming a statement (equation could not be rendered, see API doc on website)
#' 
#' * Additive: (equation could not be rendered, see API doc on website)
#' * Proportional: (equation could not be rendered, see API doc on website)
#' * Exponential: (equation could not be rendered, see API doc on website)
#' * Logit: (equation could not be rendered, see API doc on website)
#' * Rescaled logit: (equation could not be rendered, see API doc on website)
#' with (equation could not be rendered, see API doc on website)
#' 
#' @param model (Model) Pharmpy model to add new IIVs to.
#' @param list_of_parameters (array(str) or str) Name/names of parameter to add new IIVs to.
#' @param expression (array(str) or str) Effect/effects on eta. Either abbreviated (see above) or custom.
#' @param operation (str) Whether the new IIV should be added or multiplied (default).
#' @param initial_estimate (numeric) Value of initial estimate of parameter. Default is 0.09
#' @param eta_names (array(str) (optional)) Custom name/names of new eta
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- remove_iiv(model, "CL")
#' model <- add_iiv(model, "CL", "add")
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
	list_of_parameters <- convert_input(list_of_parameters, "list")
	expression <- convert_input(expression, "list")
	eta_names <- convert_input(eta_names, "list")
	func_out <- pharmpy$modeling$add_iiv(model, list_of_parameters, expression, operation=operation, initial_estimate=initial_estimate, eta_names=eta_names)
	return(py_to_r(func_out))
}

#' @title
#' add_indirect_effect
#' 
#' @description
#' Add indirect (turnover) effect
#' 
#' The concentration (equation could not be rendered, see API doc on website)
#' 
#' * Production:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Degradation:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' (equation could not be rendered, see API doc on website)
#' Baseline (equation could not be rendered, see API doc on website)
#' 
#' Models:
#' 
#' * Linear:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Emax:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Sigmoidal:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' 
#' @param model (Model) Pharmpy model
#' @param expr (str) Production (TRUE) (default) or degradation (FALSE)
#' @param prod (logical) Name of PD effect function.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- add_indirect_effect(model, expr='linear', prod=TRUE)
#' }
#' 
#' @export
add_indirect_effect <- function(model, expr, prod=TRUE) {
	func_out <- pharmpy$modeling$add_indirect_effect(model, expr, prod=prod)
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- add_individual_parameter(model, "KA")
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
#' @param list_of_parameters (array(str) or str (optional)) List of names of parameters and random variables. Accepts random variable names, parameter
#' names, or a mix of both.
#' @param eta_names (array(str) or str (optional)) Custom names of new etas. Must be equal to the number of input etas times the number of
#' categories for occasion.
#' @param distribution (str) The distribution that should be used for the new etas. Options are
#' 'disjoint' for disjoint normal distributions, 'joint' for joint normal
#' distribution, 'explicit' for an explicit mix of joint and disjoint
#' distributions, and 'same-as-iiv' for copying the distribution of IIV etas.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- add_iov(model, "TIME", "CL")
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
	list_of_parameters <- convert_input(list_of_parameters, "list")
	eta_names <- convert_input(eta_names, "list")
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- add_lag_time(model)
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
#' add_metabolite
#' 
#' @description
#' Adds a metabolite compartment to a model
#' 
#' The flow from the central compartment to the metabolite compartment
#' will be unidirectional.
#' 
#' Presystemic indicate that the metabolite compartment will be
#' directly connected to the DEPOT. If a depot compartment is not present,
#' one will be created.
#' 
#' @param model (Model) Pharmpy model
#' @param drug_dvid (numeric) DVID for drug (assuming all other DVIDs being for metabolites)
#' @param presystemic (logical) Decide wether or not to add metabolite as a presystemetic fixed drug.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- add_metabolite(model)
#' }
#' 
#' @export
add_metabolite <- function(model, drug_dvid=1, presystemic=FALSE) {
	drug_dvid <- convert_input(drug_dvid, "int")
	func_out <- pharmpy$modeling$add_metabolite(model, drug_dvid=drug_dvid, presystemic=presystemic)
	return(py_to_r(func_out))
}

#' @title
#' add_parameter_uncertainty_step
#' 
#' @description
#' Adds parameter uncertainty step to the final estimation step
#' 
#' @param model (Model) Pharmpy model
#' @param parameter_uncertainty_method (str) Parameter uncertainty method to use
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_estimation_step(model, 'FOCE', parameter_uncertainty_method=NULL)
#' model <- add_parameter_uncertainty_step(model, 'SANDWICH')
#' ests <- model$execution_steps
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
#' remove_parameter_uncertainty_step
#' 
#' set_evaluation_step
#' 
#' 
#' @export
add_parameter_uncertainty_step <- function(model, parameter_uncertainty_method) {
	func_out <- pharmpy$modeling$add_parameter_uncertainty_step(model, parameter_uncertainty_method)
	return(py_to_r(func_out))
}

#' @title
#' add_pd_iiv
#' 
#' @description
#' Adds IIVs to all PD parameters in :class:`pharmpy.model`.
#' 
#' @param model (Model) Pharmpy model to add new IIVs to.
#' @param initial_estimate (numeric) Value of initial estimate of parameter. Default is 0.09
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_direct_effect(model, 'emax')
#' model$statements$find_assignment("EC_50")
#' model <- add_pd_iiv(model)
#' model$statements$find_assignment("EC_50")
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
add_pd_iiv <- function(model, initial_estimate=0.09) {
	func_out <- pharmpy$modeling$add_pd_iiv(model, initial_estimate=initial_estimate)
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
#' If name is set, the peripheral compartment will be added to the compartment
#' with the specified name instead.
#' 
#' Initial estimates:
#' 
#' ==  ===================================================
#' n
#' ==  ===================================================
#' 1   (equation could not be rendered, see API doc on website)
#' 2   (equation could not be rendered, see API doc on website)
#' ==  ===================================================
#' 
#' @param model (Model) Pharmpy model
#' @param name (str) Name of compartment to add peripheral to.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- add_peripheral_compartment(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_peripheral_compartment
#' 
#' remove_peripheral_compartment
#' 
#' 
#' @export
add_peripheral_compartment <- function(model, name=NULL) {
	func_out <- pharmpy$modeling$add_peripheral_compartment(model, name=name)
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_first_order_absorption(model)
#' model$statements$find_assignment("MAT")
#' model <- add_pk_iiv(model)
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
#' @param lower (numeric (optional)) Lower bound of the new parameter
#' @param upper (numeric (optional)) Upper bound of the new parameter
#' @param fix (logical) Should the new parameter be fixed?
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- add_population_parameter(model, 'POP_KA', 2)
#' model$parameters
#' }
#' 
#' @export
add_population_parameter <- function(model, name, init, lower=NULL, upper=NULL, fix=FALSE) {
	func_out <- pharmpy$modeling$add_population_parameter(model, name, init, lower=lower, upper=upper, fix=fix)
	return(py_to_r(func_out))
}

#' @title
#' add_predictions
#' 
#' @description
#' Add predictions and/or residuals
#' 
#' Add predictions to estimation step.
#' 
#' @param model (Model) Pharmpy model
#' @param pred (array(str)) List of predictions (e.g. c('IPRED', 'PRED'))
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$execution_steps[-1].predictions
#' model <- add_predictions(model, c('CIPREDI'))
#' model$execution_steps[-1].predictions
#' }
#' @seealso
#' remove_predictions
#' 
#' remove_residuals
#' 
#' set_estimation_step
#' 
#' add_estimation_step
#' 
#' remove_estimation_step
#' 
#' append_estimation_step_options
#' 
#' add_parameter_uncertainty_step
#' 
#' remove_parameter_uncertainty_step
#' 
#' 
#' @export
add_predictions <- function(model, pred) {
	pred <- convert_input(pred, "list")
	func_out <- pharmpy$modeling$add_predictions(model, pred)
	return(py_to_r(func_out))
}

#' @title
#' add_residuals
#' 
#' @description
#' Add predictions and/or residuals
#' 
#' Add residuals to estimation step.
#' 
#' Added redidual variable(s) need to be one of the following :
#' c('RES', 'IRES', 'WRES', 'IWRES', 'CWRES')
#' 
#' @param model (Model) Pharmpy model
#' @param res (array(str)) List of residuals (e.g. c('CWRES'))
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$execution_steps[-1].residuals
#' model <- add_residuals(model, c('RES'))
#' model$execution_steps[-1].residuals
#' }
#' @seealso
#' remove_predictions
#' 
#' remove_residuals
#' 
#' set_estimation_step
#' 
#' add_estimation_step
#' 
#' remove_estimation_step
#' 
#' append_estimation_step_options
#' 
#' add_parameter_uncertainty_step
#' 
#' remove_parameter_uncertainty_step
#' 
#' 
#' @export
add_residuals <- function(model, res) {
	res <- convert_input(res, "list")
	func_out <- pharmpy$modeling$add_residuals(model, res)
	return(py_to_r(func_out))
}

#' @title
#' add_time_after_dose
#' 
#' @description
#' Calculate and add a TAD column to the dataset
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- add_time_after_dose(model)
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
#' @param tool_options (list(str=any)) any additional tool specific options
#' @param idx (numeric) index of estimation step (starting from 0)
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' opts <- list('NITER'=1000, 'ISAMPLE'=100)
#' model <- append_estimation_step_options(model, tool_options=opts, idx=0)
#' est <- model$execution_steps[1]
#' length(est$tool_options)
#' }
#' @seealso
#' add_estimation_step
#' 
#' set_estimation_step
#' 
#' remove_estimation_step
#' 
#' add_parameter_uncertainty_step
#' 
#' remove_parameter_uncertainty_step
#' 
#' set_evaluation_step
#' 
#' 
#' @export
append_estimation_step_options <- function(model, tool_options, idx) {
	idx <- convert_input(idx, "int")
	func_out <- pharmpy$modeling$append_estimation_step_options(model, tool_options, idx)
	return(py_to_r(func_out))
}

#' @title
#' bin_observations
#' 
#' @description
#' Bin all observations on the independent variable
#' 
#' Available binning methods:
#' 
#' +---------------+-------------------------------------------------+
#' | Method        | Description                                     |
#' +===============+=================================================+
#' | equal_width   | Bins with equal width based on the idv          |
#' +---------------+-------------------------------------------------+
#' | equal_number  | Bins containing an equal number of observations |
#' +---------------+-------------------------------------------------+
#' 
#' @param model (Model) Pharmpy model
#' @param method (str) Name of the binning method to use
#' @param nbins (numeric) The number of bins wanted
#'  
#' @return (data.frame) A series of bin ids indexed on the original record index of the dataset vector A vector of bin edges
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' bins, boundaries <- bin_observations(model, method="equal_width", nbins=10)
#' bins
#' boundaries
#' }
#' 
#' @export
bin_observations <- function(model, method, nbins) {
	nbins <- convert_input(nbins, "int")
	func_out <- pharmpy$modeling$bin_observations(model, method, nbins)
	func_out <- reset_index_df(func_out)
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
#' @param path (str (optional)) Default is to not look for files.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- model$replace(name="run2")
#' model <- bump_model_number(model)
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
#' | BIC = -2LL + n_estimated_parameters * log(n_individuals)
#' * | iiv
#' | BIC = -2LL + n_estimated_iiv_omega_parameters * log(n_individuals)
#' 
#' If multiple_testing option is set to true an additional penalty will be added:
#' 
#' * | mBIC = BIC + 2*(n_estimated_parameters)*log(n_predictors/n_expected_predictors)
#' 
#' @param model (Model) Pharmpy model object
#' @param likelihood (numeric) -2LL to use
#' @param type (str) Type of BIC to calculate. Default is the mixed effects.
#' @param multiple_testing (logical) Whether to use penalty for multiple testing (default is FALSE)
#' @param mult_test_p (numeric) Number of expected models if using type `multiple_testing`
#' @param mult_test_e (numeric) E value if using type `mult_test`
#'  
#' @return (numeric) BIC of model fit
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' ofv <- results$ofv
#' calculate_bic(model, ofv)
#' calculate_bic(model, ofv, type='fixed')
#' calculate_bic(model, ofv, type='random')
#' calculate_bic(model, ofv, type='iiv')
#' }
#' 
#' @export
calculate_bic <- function(model, likelihood, type='mixed', multiple_testing=FALSE, mult_test_p=1, mult_test_e=1) {
	mult_test_p <- convert_input(mult_test_p, "int")
	mult_test_e <- convert_input(mult_test_e, "int")
	func_out <- pharmpy$modeling$calculate_bic(model, likelihood, type=type, multiple_testing=multiple_testing, mult_test_p=mult_test_p, mult_test_e=mult_test_e)
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
#' results <- load_example_modelfit_results("pheno")
#' cov <- results$covariance_matrix
#' cov
#' calculate_corr_from_cov(cov)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_se_from_prec : Standard errors from precision matrix
#' 
#' calculate_cov_from_prec : Covariance matrix from precision matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_prec_from_cov : Precision matrix from covariance matrix
#' 
#' calculate_prec_from_corrse : Precision matrix from correlation matrix and standard errors
#' 
#' calculate_corr_from_prec : Correlation matrix from precision matrix
#' 
#' 
#' @export
calculate_corr_from_cov <- function(cov) {
	func_out <- pharmpy$modeling$calculate_corr_from_cov(cov)
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' calculate_corr_from_prec
#' 
#' @description
#' Calculate correlation matrix from a precision matrix
#' 
#' @param precision_matrix (data.frame) Precision matrix
#'  
#' @return (data.frame) Correlation matrix
#' 
#' @examples
#' \dontrun{
#' results <- load_example_modelfit_results("pheno")
#' prec <- results$precision_matrix
#' prec
#' calculate_corr_from_prec(prec)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_se_from_prec : Standard errors from precision matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_prec : Covariance matrix from precision matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_prec_from_cov : Precision matrix from covariance matrix
#' 
#' calculate_prec_from_corrse : Precision matrix from correlation matrix and standard errors
#' 
#' 
#' @export
calculate_corr_from_prec <- function(precision_matrix) {
	func_out <- pharmpy$modeling$calculate_corr_from_prec(precision_matrix)
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' calculate_cov_from_corrse
#' 
#' @description
#' Calculate covariance matrix from a correlation matrix and standard errors
#' 
#' @param corr (data.frame) Correlation matrix
#' @param se (array) Standard errors
#'  
#' @return (data.frame) Covariance matrix
#' 
#' @examples
#' \dontrun{
#' results <- load_example_modelfit_results("pheno")
#' corr <- results$correlation_matrix
#' se <- results$standard_errors
#' corr
#' calculate_cov_from_corrse(corr, se)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_se_from_prec : Standard errors from precision matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_prec : Covariance matrix from precision matrix
#' 
#' calculate_prec_from_cov : Precision matrix from covariance matrix
#' 
#' calculate_prec_from_corrse : Precision matrix from correlation matrix and standard errors
#' 
#' calculate_corr_from_prec : Correlation matrix from precision matrix
#' 
#' 
#' @export
calculate_cov_from_corrse <- function(corr, se) {
	se <- convert_input(se, "pd.Series")
	func_out <- pharmpy$modeling$calculate_cov_from_corrse(corr, se)
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' calculate_cov_from_prec
#' 
#' @description
#' Calculate covariance matrix from a precision matrix
#' 
#' @param precision_matrix (data.frame) Precision matrix
#'  
#' @return (data.frame) Covariance matrix
#' 
#' @examples
#' \dontrun{
#' results <- load_example_modelfit_results("pheno")
#' prec <- results$precision_matrix
#' prec
#' calculate_cov_from_prec(prec)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_se_from_prec : Standard errors from precision matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_prec_from_cov : Precision matrix from covariance matrix
#' 
#' calculate_prec_from_corrse : Precision matrix from correlation matrix and standard errors
#' 
#' calculate_corr_from_prec : Correlation matrix from precision matrix
#' 
#' 
#' @export
calculate_cov_from_prec <- function(precision_matrix) {
	func_out <- pharmpy$modeling$calculate_cov_from_prec(precision_matrix)
	func_out <- reset_index_df(func_out)
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
#' @param parameter_estimates (array) Parameter estimates
#' @param individual_estimates (data.frame) Table of individual (eta) estimates
#' @param sd (logical) Calculate shrinkage on the standard deviation scale (default is to calculate on the
#' variance scale)
#'  
#' @return (Series) Shrinkage for each eta
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' pe <- results$parameter_estimates
#' ie <- results$individual_estimates
#' calculate_eta_shrinkage(model, pe, ie)
#' calculate_eta_shrinkage(model, pe, ie, sd=TRUE)
#' }
#' @seealso
#' calculate_individual_shrinkage
#' 
#' 
#' @export
calculate_eta_shrinkage <- function(model, parameter_estimates, individual_estimates, sd=FALSE) {
	parameter_estimates <- convert_input(parameter_estimates, "pd.Series")
	func_out <- pharmpy$modeling$calculate_eta_shrinkage(model, parameter_estimates, individual_estimates, sd=sd)
	func_out <- reset_index_df(func_out)
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
#' @param expr_or_exprs (array(BooleanExpr) or array(Expr) or array(str) or BooleanExpr or Expr or str) Parameter estimates
#' @param parameter_estimates (array) Parameter uncertainty covariance matrix
#' @param covariance_matrix (data.frame (optional)) expression or iterable of str or expressions
#' Expressions or equations for parameters of interest. If equations are used
#' the names of the left hand sides will be used as the names of the parameters.
#' @param seed (numeric (optional)) Random number generator or integer seed
#'  
#' @return (data.frame) A DataFrame of statistics indexed on parameter and covariate value.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' rng <- create_rng(23)
#' pe <- results$parameter_estimates
#' cov <- results$covariance_matrix
#' calculate_individual_parameter_statistics(model, "K=CL/V", pe, cov, seed=rng)
#' }
#' 
#' @export
calculate_individual_parameter_statistics <- function(model, expr_or_exprs, parameter_estimates, covariance_matrix=NULL, seed=NULL) {
	parameter_estimates <- convert_input(parameter_estimates, "pd.Series")
	seed <- convert_input(seed, "int")
	func_out <- pharmpy$modeling$calculate_individual_parameter_statistics(model, expr_or_exprs, parameter_estimates, covariance_matrix=covariance_matrix, seed=seed)
	func_out <- reset_index_df(func_out)
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
#' @param parameter_estimates (array) Parameter estimates of model
#' @param individual_estimates_covariance (data.frame) Uncertainty covariance matrices of individual estimates
#'  
#' @return (DataFrame) Shrinkage for each eta and individual
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' pe <- results$parameter_estimates
#' covs <- results$individual_estimates_covariance
#' calculate_individual_shrinkage(model, pe, covs)
#' }
#' @seealso
#' calculate_eta_shrinkage
#' 
#' 
#' @export
calculate_individual_shrinkage <- function(model, parameter_estimates, individual_estimates_covariance) {
	parameter_estimates <- convert_input(parameter_estimates, "pd.Series")
	func_out <- pharmpy$modeling$calculate_individual_shrinkage(model, parameter_estimates, individual_estimates_covariance)
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' calculate_parameters_from_ucp
#' 
#' @description
#' Scale parameter values from ucp to normal scale
#' 
#' @param model (Model) Pharmpy model
#' @param scale (UCPScale) A parameter scale
#' @param ucps (array or list(str=numeric)) Series of parameter values
#'  
#' @return (data.frame) Parameters on the normal scale
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' scale <- calculate_ucp_scale(model)
#' values <- {'PTVCL': 0.1, 'PTVV': 0.1, 'THETA_3': 0.1, 'IVCL': 0.1, 'IVV': 0.1, 'SIGMA_1_1': 0.1}
#' calculate_parameters_from_ucp(model, scale, values)
#' }
#' @seealso
#' calculate_ucp_scale : Calculate the scale for conversion from ucps
#' 
#' 
#' @export
calculate_parameters_from_ucp <- function(model, scale, ucps) {
	func_out <- pharmpy$modeling$calculate_parameters_from_ucp(model, scale, ucps)
	func_out <- reset_index_df(func_out)
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
#' @param parameter_estimates (array) Parameter estimates
#' @param covariance_matrix (data.frame (optional)) Parameter uncertainty covariance matrix
#' @param seed (numeric (optional)) Random number generator or seed
#'  
#' @return (data.frame) A DataFrame of statistics indexed on parameter and covariate value.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' rng <- create_rng(23)
#' pe <- results$parameter_estimates
#' cov <- results$covariance_matrix
#' calculate_pk_parameters_statistics(model, pe, cov, seed=rng)
#' }
#' @seealso
#' calculate_individual_parameter_statistics : Calculation of statistics for arbitrary parameters
#' 
#' 
#' @export
calculate_pk_parameters_statistics <- function(model, parameter_estimates, covariance_matrix=NULL, seed=NULL) {
	parameter_estimates <- convert_input(parameter_estimates, "pd.Series")
	seed <- convert_input(seed, "int")
	func_out <- pharmpy$modeling$calculate_pk_parameters_statistics(model, parameter_estimates, covariance_matrix=covariance_matrix, seed=seed)
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' calculate_prec_from_corrse
#' 
#' @description
#' Calculate precision matrix from a correlation matrix and standard errors
#' 
#' @param corr (data.frame) Correlation matrix
#' @param se (array) Standard errors
#'  
#' @return (data.frame) Precision matrix
#' 
#' @examples
#' \dontrun{
#' results <- load_example_modelfit_results("pheno")
#' corr <- results$correlation_matrix
#' se <- results$standard_errors
#' corr
#' calculate_prec_from_corrse(corr, se)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_se_from_prec : Standard errors from precision matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_prec : Covariance matrix from precision matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_prec_from_cov : Precision matrix from covariance matrix
#' 
#' calculate_corr_from_prec : Correlation matrix from precision matrix
#' 
#' 
#' @export
calculate_prec_from_corrse <- function(corr, se) {
	se <- convert_input(se, "pd.Series")
	func_out <- pharmpy$modeling$calculate_prec_from_corrse(corr, se)
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' calculate_prec_from_cov
#' 
#' @description
#' Calculate precision matrix from a covariance matrix
#' 
#' @param cov (data.frame) Covariance matrix
#'  
#' @return (data.frame) Precision matrix
#' 
#' @examples
#' \dontrun{
#' results <- load_example_modelfit_results("pheno")
#' cov <- results$covariance_matrix
#' cov
#' calculate_prec_from_cov(cov)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_se_from_prec : Standard errors from precision matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_prec : Covariance matrix from precision matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_prec_from_corrse : Precision matrix from correlation matrix and standard errors
#' 
#' calculate_corr_from_prec : Correlation matrix from precision matrix
#' 
#' 
#' @export
calculate_prec_from_cov <- function(cov) {
	func_out <- pharmpy$modeling$calculate_prec_from_cov(cov)
	func_out <- reset_index_df(func_out)
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
#' results <- load_example_modelfit_results("pheno")
#' cov <- results$covariance_matrix
#' cov
#' calculate_se_from_cov(cov)
#' }
#' @seealso
#' calculate_se_from_prec : Standard errors from precision matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_prec : Covariance matrix from precision matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_prec_from_cov : Precision matrix from covariance matrix
#' 
#' calculate_prec_from_corrse : Precision matrix from correlation matrix and standard errors
#' 
#' calculate_corr_from_prec : Correlation matrix from precision matrix
#' 
#' 
#' @export
calculate_se_from_cov <- function(cov) {
	func_out <- pharmpy$modeling$calculate_se_from_cov(cov)
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' calculate_se_from_prec
#' 
#' @description
#' Calculate standard errors from a precision matrix
#' 
#' @param precision_matrix (data.frame) Input precision matrix
#'  
#' @return (data.frame) Standard errors
#' 
#' @examples
#' \dontrun{
#' results <- load_example_modelfit_results("pheno")
#' prec <- results$precision_matrix
#' prec
#' calculate_se_from_prec(prec)
#' }
#' @seealso
#' calculate_se_from_cov : Standard errors from covariance matrix
#' 
#' calculate_corr_from_cov : Correlation matrix from covariance matrix
#' 
#' calculate_cov_from_prec : Covariance matrix from precision matrix
#' 
#' calculate_cov_from_corrse : Covariance matrix from correlation matrix and standard errors
#' 
#' calculate_prec_from_cov : Precision matrix from covariance matrix
#' 
#' calculate_prec_from_corrse : Precision matrix from correlation matrix and standard errors
#' 
#' calculate_corr_from_prec : Correlation matrix from precision matrix
#' 
#' 
#' @export
calculate_se_from_prec <- function(precision_matrix) {
	func_out <- pharmpy$modeling$calculate_se_from_prec(precision_matrix)
	func_out <- reset_index_df(func_out)
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
#' @param dataframe (logical) TRUE to return a DataFrame instead of printing to the console
#' @param verbose (logical) Print out all rules checked if TRUE else print only failed rules
#'  
#' @return (data.frame) Only returns a DataFrame is dataframe=TRUE
#' 
#' 
#' @export
check_dataset <- function(model, dataframe=FALSE, verbose=FALSE) {
	func_out <- pharmpy$modeling$check_dataset(model, dataframe=dataframe, verbose=verbose)
	func_out <- reset_index_df(func_out)
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
#' results <- load_example_modelfit_results("pheno")
#' cor <- results$correlation_matrix
#' check_high_correlations(model, cor, limit=0.3)
#' }
#' 
#' @export
check_high_correlations <- function(model, cor, limit=0.9) {
	func_out <- pharmpy$modeling$check_high_correlations(model, cor, limit=limit)
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' check_parameters_near_bounds
#' 
#' @description
#' Check if any estimated parameter value is close to its bounds
#' 
#' @param model (Model) Pharmpy model object
#' @param values (array) Series of values with index a subset of parameter names.
#' @param zero_limit (numeric) maximum distance to 0 bounds
#' @param significant_digits (numeric) maximum distance to non-zero bounds in number of significant digits
#'  
#' @return (data.frame) Logical Series with same index as values
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' check_parameters_near_bounds(model, results$parameter_estimates)
#' }
#' 
#' @export
check_parameters_near_bounds <- function(model, values, zero_limit=0.001, significant_digits=2) {
	values <- convert_input(values, "pd.Series")
	significant_digits <- convert_input(significant_digits, "int")
	func_out <- pharmpy$modeling$check_parameters_near_bounds(model, values, zero_limit=zero_limit, significant_digits=significant_digits)
	func_out <- reset_index_df(func_out)
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
#' @param model (Model) Pharmpy model object
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements
#' model <- cleanup_model(model)
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
#' @param to_format (str) Name of format to convert into. Currently supported 'generic', 'nlmixr', 'nonmem', and 'rxode'
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
#' create_basic_pk_model
#' 
#' @description
#' Creates a basic pk model of given type. The model will be a one compartment model, with first
#' order elimination and in the case of oral administration first order absorption with no absorption
#' delay. The elimination rate will be (equation could not be rendered, see API doc on website)
#' 
#' @param administration (str) Type of PK model to create. Supported are 'iv', 'oral' and 'ivoral'
#' @param dataset_path (str (optional)) Optional path to a dataset
#' @param cl_init (numeric) Initial estimate of the clearance parameter
#' @param vc_init (numeric) Initial estimate of the central volume parameter
#' @param mat_init (numeric) Initial estimate of the mean absorption time parameter (if applicable)
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- create_basic_pk_model('oral')
#' }
#' 
#' @export
create_basic_pk_model <- function(administration='iv', dataset_path=NULL, cl_init=0.01, vc_init=1.0, mat_init=0.1) {
	func_out <- pharmpy$modeling$create_basic_pk_model(administration=administration, dataset_path=dataset_path, cl_init=cl_init, vc_init=vc_init, mat_init=mat_init)
	return(py_to_r(func_out))
}

#' @title
#' create_config_template
#' 
#' @description
#' Create a basic config file template
#' 
#' If a configuration file already exists it will not be overwritten
#' 
#' @examples
#' \dontrun{
#' create_config_template()
#' }
#' 
#' @export
create_config_template <- function() {
	func_out <- pharmpy$modeling$create_config_template()
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
#' @param rvs (array(str) (optional)) Sequence of etas or names of etas to combine. If NULL, all etas that are IIVs and
#' non-fixed will be used (full block). NULL is default.
#' @param individual_estimates (data.frame (optional)) Optional individual estimates to use for calculation of initial estimates
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$random_variables$etas
#' model <- create_joint_distribution(model, c('ETA_1', 'ETA_2'))
#' model$random_variables$etas
#' }
#' @seealso
#' split_joint_distribution : split etas into separate distributions
#' 
#' 
#' @export
create_joint_distribution <- function(model, rvs=NULL, individual_estimates=NULL) {
	rvs <- convert_input(rvs, "list")
	func_out <- pharmpy$modeling$create_joint_distribution(model, rvs=rvs, individual_estimates=individual_estimates)
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
#' @param seed (numeric (optional)) Seed for the random number generator or NULL (default) for a randomized seed. If seed
#' is generator it will be passed through.
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
	seed <- convert_input(seed, "int")
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
#' COVEFF → COVEFF1
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
#' @param date_columns (array(str) (optional)) Names of all date columns
#'  
#' @return (data.frame) Deidentified dataset
#' 
#' 
#' @export
deidentify_data <- function(df, id_column='ID', date_columns=NULL) {
	date_columns <- convert_input(date_columns, "list")
	func_out <- pharmpy$modeling$deidentify_data(df, id_column=id_column, date_columns=date_columns)
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' display_odes
#' 
#' @description
#' Displays the ordinary differential equation system
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (ODEDisplayer) A displayable object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' display_odes(model)
#' }
#' 
#' @export
display_odes <- function(model) {
	func_out <- pharmpy$modeling$display_odes(model)
	return(py_to_r(func_out))
}

#' @title
#' drop_columns
#' 
#' @description
#' Drop columns from the dataset or mark as dropped
#' 
#' @param model (Model) Pharmpy model object
#' @param column_names (array(str) or str) List of column names or one column name to drop or mark as dropped
#' @param mark (logical) Default is to remove column from dataset. Set this to TRUE to only mark as dropped
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- drop_columns(model, c('WGT', 'APGR'))
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
	column_names <- convert_input(column_names, "list")
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- drop_dropped_columns(model)
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
#' @param etas (data.frame (optional)) Optional list of eta values
#' @param parameters (list(str=numeric) (optional)) Optional list of parameters and values
#' @param dataset (data.frame (optional)) Optional dataset
#'  
#' @return (data.frame) Gradient
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' results <- load_example_modelfit_results("pheno_linear")
#' etas <- results$individual_estimates
#' evaluate_epsilon_gradient(model, etas=etas)
#' }
#' @seealso
#' evaluate_eta_gradient : Evaluate the eta gradient
#' 
#' 
#' @export
evaluate_epsilon_gradient <- function(model, etas=NULL, parameters=NULL, dataset=NULL) {
	parameters <- convert_input(parameters, "Mapping")
	func_out <- pharmpy$modeling$evaluate_epsilon_gradient(model, etas=etas, parameters=parameters, dataset=dataset)
	func_out <- reset_index_df(func_out)
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
#' @param etas (data.frame (optional)) Optional list of eta values
#' @param parameters (list(str=numeric) (optional)) Optional list of parameters and values
#' @param dataset (data.frame (optional)) Optional dataset
#'  
#' @return (data.frame) Gradient
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' results <- load_example_modelfit_results("pheno_linear")
#' etas <- results$individual_estimates
#' evaluate_eta_gradient(model, etas=etas)
#' }
#' @seealso
#' evaluate_epsilon_gradient : Evaluate the epsilon gradient
#' 
#' 
#' @export
evaluate_eta_gradient <- function(model, etas=NULL, parameters=NULL, dataset=NULL) {
	parameters <- convert_input(parameters, "Mapping")
	func_out <- pharmpy$modeling$evaluate_eta_gradient(model, etas=etas, parameters=parameters, dataset=dataset)
	func_out <- reset_index_df(func_out)
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
#' @param expression (str or numeric or Expr) Expression to evaluate
#' @param parameter_estimates (list(str=numeric) (optional)) Parameter estimates to use instead of initial estimates
#'  
#' @return (data.frame) A series of one evaluated value for each data record
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' pe <- results$parameter_estimates
#' evaluate_expression(model, "TVCL*1000", parameter_estimates=pe)
#' }
#' 
#' @export
evaluate_expression <- function(model, expression, parameter_estimates=NULL) {
	parameter_estimates <- convert_input(parameter_estimates, "Mapping")
	func_out <- pharmpy$modeling$evaluate_expression(model, expression, parameter_estimates=parameter_estimates)
	func_out <- reset_index_df(func_out)
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
#' @param etas (data.frame (optional)) Optional list of eta values
#' @param parameters (list(str=numeric) (optional)) Optional list of parameters and values
#' @param dataset (data.frame (optional)) Optional dataset
#'  
#' @return (data.frame) Individual predictions
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' results <- load_example_modelfit_results("pheno_linear")
#' etas <- results$individual_estimates
#' evaluate_individual_prediction(model, etas=etas)
#' }
#' @seealso
#' evaluate_population_prediction : Evaluate the population prediction
#' 
#' 
#' @export
evaluate_individual_prediction <- function(model, etas=NULL, parameters=NULL, dataset=NULL) {
	parameters <- convert_input(parameters, "Mapping")
	func_out <- pharmpy$modeling$evaluate_individual_prediction(model, etas=etas, parameters=parameters, dataset=dataset)
	func_out <- reset_index_df(func_out)
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
#' @param parameters (list(str=numeric) (optional)) Optional list of parameters and values
#' @param dataset (data.frame (optional)) Optional dataset
#'  
#' @return (data.frame) Population predictions
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' results <- load_example_modelfit_results("pheno_linear")
#' pe <- results$parameter_estimates
#' evaluate_population_prediction(model, parameters=list(pe))
#' }
#' @seealso
#' evaluate_individual_prediction : Evaluate the individual prediction
#' 
#' 
#' @export
evaluate_population_prediction <- function(model, parameters=NULL, dataset=NULL) {
	parameters <- convert_input(parameters, "Mapping")
	func_out <- pharmpy$modeling$evaluate_population_prediction(model, parameters=parameters, dataset=dataset)
	func_out <- reset_index_df(func_out)
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
#' @param parameters (list(str=numeric) (optional)) Optional list of parameters and values
#' @param dataset (data.frame (optional)) Optional dataset
#'  
#' @return (data.frame) WRES
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno_linear")
#' results <- load_example_modelfit_results("pheno_linear")
#' parameters <- results$parameter_estimates
#' evaluate_weighted_residuals(model, parameters=list(parameters))
#' }
#' 
#' @export
evaluate_weighted_residuals <- function(model, parameters=NULL, dataset=NULL) {
	parameters <- convert_input(parameters, "Mapping")
	func_out <- pharmpy$modeling$evaluate_weighted_residuals(model, parameters=parameters, dataset=dataset)
	func_out <- reset_index_df(func_out)
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
#' columns in the original dataset will be kept. Care needs to be taken to handle
#' the new dataset.
#'  
#' @return (Model) Pharmpy model object
#' 
#' 
#' @export
expand_additional_doses <- function(model, flag=FALSE) {
	func_out <- pharmpy$modeling$expand_additional_doses(model, flag=flag)
	return(py_to_r(func_out))
}

#' @title
#' filter_dataset
#' 
#' @description
#' Filter dataset according to expr and return a model with the filtered dataset.
#' 
#' Example: "DVID == 1" will filter the dataset so that only the rows with DVID = 1 remain.
#' 
#' @param model (Model) Pharmpy model object
#' @param expr (str) expression for dataset query
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$dataset
#' model <- filter_dataset(model, 'WGT < 1.4')
#' model$dataset
#' }
#' 
#' @export
filter_dataset <- function(model, expr) {
	func_out <- pharmpy$modeling$filter_dataset(model, expr)
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
#' @param parameters (list(str=logical)) Set fix/unfix for these parameters
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$parameters['PTVCL']
#' model <- fix_or_unfix_parameters(model, list('PTVCL'=TRUE))
#' model$parameters['PTVCL']
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
#' @param parameter_names (array(str) or str) one parameter name or a vector of parameter names
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$parameters['PTVCL']
#' model <- fix_parameters(model, 'PTVCL')
#' model$parameters['PTVCL']
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
#' @param inits (list(str=numeric)) Inits for all parameters to fix and set init
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$parameters['PTVCL']
#' model <- fix_parameters_to(model, {'PTVCL': 0.5})
#' model$parameters['PTVCL']
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
#' get_admid
#' 
#' @description
#' Get the admid from model dataset
#' 
#' If an administration column is present this will be extracted otherwise
#' an admid column will be created based on the admids of the present doses.
#' This is dependent on the presence of a CMT column to be generated correctly.
#' 
#' When generated, admids of events in between doses is set to the last used
#' admid.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (data.frame) ADMID
#' 
#' 
#' @export
get_admid <- function(model) {
	func_out <- pharmpy$modeling$get_admid(model)
	func_out <- reset_index_df(func_out)
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
	func_out <- reset_index_df(func_out)
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
#' get_central_volume_and_clearance
#' 
#' @description
#' Get the volume and clearance parameters
#' 
#' @param model (Model) Pharmpy model
#' 
#'  
#' @return (sympy.Symbol) Volume symbol sympy.Symbol Clearance symbol
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_central_volume_and_clearance(model)
#' }
#' 
#' @export
get_central_volume_and_clearance <- function(model) {
	func_out <- pharmpy$modeling$get_central_volume_and_clearance(model)
	return(py_to_r(func_out))
}

#' @title
#' get_cmt
#' 
#' @description
#' Get the cmt (compartment) column from the model dataset
#' 
#' If a cmt column is present this will be extracted otherwise
#' a cmt column will be created. If created, multiple dose compartments are
#' dependent on the presence of an admid type column, otherwise, dose/non-dose
#' will be considered.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (data.frame) CMT
#' 
#' 
#' @export
get_cmt <- function(model) {
	func_out <- pharmpy$modeling$get_cmt(model)
	func_out <- reset_index_df(func_out)
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
	func_out <- reset_index_df(func_out)
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
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' get_covariate_effects
#' 
#' @description
#' Return a list of all used covariates within a model
#' 
#' The list will have parameter name as key with a connected value as
#' a vector of tuple(s) with (covariate, effect type, operator)
#' 
#' @param model (Model) Model to extract covariates from.
#'  
#' @return (Dictionary : Dictionary of parameters and connected covariate(s)) 
#' 
#' 
#' @export
get_covariate_effects <- function(model) {
	func_out <- pharmpy$modeling$get_covariate_effects(model)
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
	func_out <- reset_index_df(func_out)
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
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' get_dv_symbol
#' 
#' @description
#' Get the symbol for a certain dvid or dv and check that it is valid
#' 
#' @param model (Model) Pharmpy model
#' @param dv (Expr or str or numeric (optional)) Either a dv symbol, str or dvid. If NULL (default) return the
#' only or first dv.
#'  
#' @return (sympy.Symbol) DV symbol
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_dv_symbol(model, "Y")
#' get_dv_symbol(model, 1)
#' }
#' 
#' @export
get_dv_symbol <- function(model, dv=NULL) {
	func_out <- pharmpy$modeling$get_dv_symbol(model, dv=dv)
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
	func_out <- reset_index_df(func_out)
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
#' Retrieves all individual parameters in a :class:`pharmpy.model`.
#' 
#' By default all individual parameters will be found even ones having no random effect. The level
#' arguments makes it possible to find only those having any random effect or only those having a certain
#' random effect. Using the dv option will give all individual parameters affecting a certain dv. Note that
#' the DV for PD in a PKPD model often also is affected by the PK parameters.
#' 
#' @param model (Model) Pharmpy model to retrieve the individuals parameters from
#' @param level (str) The variability level to look for: 'iiv', 'iov', 'random' or 'all' (default)
#' @param dv (str or Expr or numeric (optional)) Name or DVID of dependent variable. NULL for all (default)
#'  
#' @return (vectorc(str)) A vector of the parameter names as strings
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_individual_parameters(model)
#' get_individual_parameters(model, 'iiv')
#' get_individual_parameters(model, 'iov')
#' }
#' @seealso
#' get_pd_parameters
#' 
#' get_pk_parameters
#' 
#' get_rv_parameters
#' 
#' has_random_effect
#' 
#' 
#' @export
get_individual_parameters <- function(model, level='all', dv=NULL) {
	func_out <- pharmpy$modeling$get_individual_parameters(model, level=level, dv=dv)
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
#' get_initial_conditions
#' 
#' @description
#' Get initial conditions for the ode system
#' 
#' Default initial conditions at t=0 for amounts is 0
#' 
#' @param model (Model) Pharmpy model
#' @param dosing (logical) Set to TRUE to add dosing as initial conditions
#'  
#' @return (list) Initial conditions
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_initial_conditions(model)
#' get_initial_conditions(model, dosing=TRUE)
#' }
#' 
#' @export
get_initial_conditions <- function(model, dosing=FALSE) {
	func_out <- pharmpy$modeling$get_initial_conditions(model, dosing=dosing)
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
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' get_model_code
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
#' get_model_code(model)
#' }
#' 
#' @export
get_model_code <- function(model) {
	func_out <- pharmpy$modeling$get_model_code(model)
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
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' get_number_of_peripheral_compartments
#' 
#' @description
#' Return the number of peripherals compartments connected to the central
#' compartment
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (integer) Number of peripherals compartments
#' 
#' 
#' @export
get_number_of_peripheral_compartments <- function(model) {
	func_out <- pharmpy$modeling$get_number_of_peripheral_compartments(model)
	return(py_to_r(func_out))
}

#' @title
#' get_number_of_transit_compartments
#' 
#' @description
#' Return the number of transit compartments in the model
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (integer) Number of transit compartments
#' 
#' 
#' @export
get_number_of_transit_compartments <- function(model) {
	func_out <- pharmpy$modeling$get_number_of_transit_compartments(model)
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
#' print(expr$unicode())
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
#' @param keep_index (logical) Set to TRUE if the original index should be kept.
#' Otherwise a new index using ID and idv will be created.
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
get_observations <- function(model, keep_index=FALSE) {
	func_out <- pharmpy$modeling$get_observations(model, keep_index=keep_index)
	func_out <- reset_index_df(func_out)
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
#' get_parameter_rv
#' 
#' @description
#' Retrieves name of random variable in :class:`pharmpy.model.Model` given a parameter.
#' 
#' @param model (Model) Pharmpy model to retrieve parameters from
#' @param parameter (str) Name of parameter to retrieve random variable from
#' @param var_type (str) Variability type: iiv (default) or iov
#'  
#' @return (vectorc(str)) A vector of random variable names for the given parameter
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_parameter_rv(model, 'CL')
#' }
#' @seealso
#' get_rv_parameters
#' 
#' has_random_effect
#' 
#' get_pk_parameters
#' 
#' get_individual_parameters
#' 
#' 
#' @export
get_parameter_rv <- function(model, parameter, var_type='iiv') {
	func_out <- pharmpy$modeling$get_parameter_rv(model, parameter, var_type=var_type)
	return(py_to_r(func_out))
}

#' @title
#' get_pd_parameters
#' 
#' @description
#' Retrieves PD parameters in :class:`pharmpy.model.Model`.
#' 
#' @param model (Model) Pharmpy model to retrieve the PD parameters from
#'  
#' @return (vectorc(str)) A vector of the PD parameter names of the given model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_direct_effect(model, "linear")
#' get_pd_parameters(model)
#' }
#' @seealso
#' get_pk_parameters
#' 
#' 
#' @export
get_pd_parameters <- function(model) {
	func_out <- pharmpy$modeling$get_pd_parameters(model)
	return(py_to_r(func_out))
}

#' @title
#' get_pk_parameters
#' 
#' @description
#' Retrieves PK parameters in :class:`pharmpy.model.Model`.
#' 
#' @param model (Model) Pharmpy model to retrieve the PK parameters from
#' @param kind (str) The type of parameter to retrieve: 'absorption', 'distribution',
#' 'elimination', or 'all' (default).
#'  
#' @return (vectorc(str)) A vector of the PK parameter names of the given model
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
#' Retrieves parameters in :class:`pharmpy.model.Model` given a random variable.
#' 
#' @param model (Model) Pharmpy model to retrieve parameters from
#' @param rv (str) Name of random variable to retrieve
#'  
#' @return (vectorc(str)) A vector of parameter names for the given random variable
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_rv_parameters(model, 'ETA_1')
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
#' @param variable (str) Find physical unit of this variable
#'  
#' @return (Unit) A unit expression
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
#' get_zero_order_inputs
#' 
#' @description
#' Get zero order inputs for all compartments
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (sympy.Matrix) Vector of inputs
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' get_zero_order_inputs(model)
#' }
#' 
#' @export
get_zero_order_inputs <- function(model) {
	func_out <- pharmpy$modeling$get_zero_order_inputs(model)
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements
#' model <- greekify_model(cleanup_model(model))
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
#' Multiple dependent variables are supported. By default the only (in case of one) or the
#' first (in case of many) dependent variable is going to be checked.
#' 
#' @param model (Model) The model to check
#' @param dv (str or Expr or numeric (optional)) Name or DVID of dependent variable. NULL for the default (first or only)
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
#' has_weighted_error_model : Check if a model has a weighted error model
#' 
#' 
#' @export
has_additive_error_model <- function(model, dv=NULL) {
	func_out <- pharmpy$modeling$has_additive_error_model(model, dv=dv)
	return(py_to_r(func_out))
}

#' @title
#' has_combined_error_model
#' 
#' @description
#' Check if a model has a combined additive and proportional error model
#' 
#' Multiple dependent variables are supported. By default the only (in case of one) or the
#' first (in case of many) dependent variable is going to be checked.
#' 
#' @param model (Model) The model to check
#' @param dv (str or Expr or numeric (optional)) Name or DVID of dependent variable. NULL for the default (first or only)
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
#' has_weighted_error_model : Check if a model has a weighted error model
#' 
#' 
#' @export
has_combined_error_model <- function(model, dv=NULL) {
	func_out <- pharmpy$modeling$has_combined_error_model(model, dv=dv)
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
#' has_first_order_absorption
#' 
#' @description
#' Check if ode system describes a first order absorption
#' 
#' Currently defined as the central compartment having a unidirectional input
#' flow from another compartment (such as depot or transit)
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Bool : TRUE if model has first order absorption) 
#' 
#' 
#' @export
has_first_order_absorption <- function(model) {
	func_out <- pharmpy$modeling$has_first_order_absorption(model)
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
#' has_instantaneous_absorption
#' 
#' @description
#' Check if ode system describes a instantaneous absorption
#' 
#' Defined as being a instantaneous dose directly into the central compartment
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Bool : TRUE if model has instantaneous absorption) 
#' 
#' 
#' @export
has_instantaneous_absorption <- function(model) {
	func_out <- pharmpy$modeling$has_instantaneous_absorption(model)
	return(py_to_r(func_out))
}

#' @title
#' has_linear_odes
#' 
#' @description
#' Check if model has a linear ODE system
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (logical) TRUE if model has an ODE system that is linear
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_linear_odes(model)
#' }
#' @seealso
#' has_odes
#' 
#' has_linear_odes_with_real_eigenvalues
#' 
#' 
#' 
#' 
#' @export
has_linear_odes <- function(model) {
	func_out <- pharmpy$modeling$has_linear_odes(model)
	return(py_to_r(func_out))
}

#' @title
#' has_linear_odes_with_real_eigenvalues
#' 
#' @description
#' Check if model has a linear ode system with real eigenvalues
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (logical) TRUE if model has an ODE system that is linear
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_linear_odes_with_real_eigenvalues(model)
#' }
#' @seealso
#' has_odes
#' 
#' has_linear_odes
#' 
#' 
#' 
#' 
#' @export
has_linear_odes_with_real_eigenvalues <- function(model) {
	func_out <- pharmpy$modeling$has_linear_odes_with_real_eigenvalues(model)
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
#' model <- set_michaelis_menten_elimination(model)
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
#' model <- set_mixed_mm_fo_elimination(model)
#' has_mixed_mm_fo_elimination(model)
#' }
#' 
#' @export
has_mixed_mm_fo_elimination <- function(model) {
	func_out <- pharmpy$modeling$has_mixed_mm_fo_elimination(model)
	return(py_to_r(func_out))
}

#' @title
#' has_odes
#' 
#' @description
#' Check if model has an ODE system
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (logical) TRUE if model has an ODE system
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_odes(model)
#' }
#' @seealso
#' has_linear_odes
#' 
#' has_linear_odes_with_real_eigenvalues
#' 
#' 
#' 
#' 
#' @export
has_odes <- function(model) {
	func_out <- pharmpy$modeling$has_odes(model)
	return(py_to_r(func_out))
}

#' @title
#' has_presystemic_metabolite
#' 
#' @description
#' Checks whether a model has a presystemic metabolite
#' 
#' If pre-systemic drug there will be a flow from DEPOT to METABOLITE as well
#' as being a flow from the CENTRAL to METABOLITE
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (logical) Whether a model has presystemic metabolite
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- add_metabolite(model, presystemic=TRUE)
#' has_presystemic_metabolite(model)
#' }
#' 
#' @export
has_presystemic_metabolite <- function(model) {
	func_out <- pharmpy$modeling$has_presystemic_metabolite(model)
	return(py_to_r(func_out))
}

#' @title
#' has_proportional_error_model
#' 
#' @description
#' Check if a model has a proportional error model
#' 
#' Multiple dependent variables are supported. By default the only (in case of one) or the
#' first (in case of many) dependent variable is going to be checked.
#' 
#' @param model (Model) The model to check
#' @param dv (str or Expr or numeric (optional)) Name or DVID of dependent variable. NULL for the default (first or only)
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
#' has_weighted_error_model : Check if a model has a weighted error model
#' 
#' 
#' @export
has_proportional_error_model <- function(model, dv=NULL) {
	func_out <- pharmpy$modeling$has_proportional_error_model(model, dv=dv)
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
#' has_seq_zo_fo_absorption
#' 
#' @description
#' Check if ode system describes a sequential zero-order, first-order absorption
#' 
#' Defined as the model having both zero- and first-order absorption.
#' 
#' @param model (Model) DPharmpy model
#'  
#' @seealso
#' has_zero_order_absorption
#' 
#' has_first_order_absorption
#' 
#' 
#' @export
has_seq_zo_fo_absorption <- function(model) {
	func_out <- pharmpy$modeling$has_seq_zo_fo_absorption(model)
	return(py_to_r(func_out))
}

#' @title
#' has_weighted_error_model
#' 
#' @description
#' Check if a model has a weighted error model
#' 
#' @param model (Model) The model to check
#'  
#' @return (logical) TRUE if the model has a weighted error model and FALSE otherwise
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_weighted_error_model(model)
#' }
#' @seealso
#' has_additive_error_model : Check if a model has an additive error model
#' 
#' has_combined_error_model : Check if a model has a combined error model
#' 
#' has_proportional_error_model : Check if a model has a proportional error model
#' 
#' 
#' @export
has_weighted_error_model <- function(model) {
	func_out <- pharmpy$modeling$has_weighted_error_model(model)
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
#' model <- set_zero_order_elimination(model)
#' has_zero_order_elimination(model)
#' }
#' 
#' @export
has_zero_order_elimination <- function(model) {
	func_out <- pharmpy$modeling$has_zero_order_elimination(model)
	return(py_to_r(func_out))
}

#' @title
#' is_linearized
#' 
#' @description
#' Determine if a model is linearized
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (logical) TRUE if model has been linearized and FALSE otherwise
#' 
#' @examples
#' \dontrun{
#' model1 <- load_example_model("pheno")
#' is_linearized(model1)
#' model2 <- load_example_model("pheno_linear")
#' is_linearized(model2)
#' }
#' 
#' @export
is_linearized <- function(model) {
	func_out <- pharmpy$modeling$is_linearized(model)
	return(py_to_r(func_out))
}

#' @title
#' is_real
#' 
#' @description
#' Determine if an expression is real valued given constraints of a model
#' 
#' @param model (Model) Pharmpy model
#' @param expr (numeric or str or Expr) Expression to test
#'  
#' @return (logical or NULL) TRUE if expression is real, FALSE if not and NULL if unknown
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' is_real(model, "CL")
#' }
#' 
#' @export
is_real <- function(model, expr) {
	func_out <- pharmpy$modeling$is_real(model, expr)
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
#' load_dataset
#' 
#' @description
#' Load the dataset given datainfo
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Pharmpy model with dataset removed
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- unload_dataset(model)
#' model$dataset is NULL
#' model <- load_dataset(model)
#' model$dataset
#' }
#' 
#' @export
load_dataset <- function(model) {
	func_out <- pharmpy$modeling$load_dataset(model)
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements$before_odes
#' model <- make_declarative(model)
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
#' For example: (equation could not be rendered, see API doc on website)
#' normal distribution would give (equation could not be rendered, see API doc on website)
#' (equation could not be rendered, see API doc on website)
#' 
#' @param model (Model) Pharmpy model object
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- mu_reference_model(model)
#' model$statements$before_odes
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
#' @return (iterator) Iterator yielding tuples of models/dataframes and the omitted group
#' 
#' 
#' @export
omit_data <- function(dataset_or_model, group, name_pattern='omitted_{}') {
	func_out <- pharmpy$modeling$omit_data(dataset_or_model, group, name_pattern=name_pattern)
	return(py_to_r(func_out))
}

#' @title
#' plot_abs_cwres_vs_ipred
#' 
#' @description
#' Plot \|CWRES\| vs IPRED
#' 
#' @param model (Model) Pharmpy model
#' @param predictions (data.frame) DataFrame containing the predictions
#' @param residuals (data.frame) DataFrame containing the residuals
#' @param stratify_on (str) Name of parameter for stratification
#' @param bins (numeric) Number of bins for stratification
#'  
#' @return (alt.Chart) Plot
#' 
#' @examples
#' \dontrun{
#' }
#' 
#' @export
plot_abs_cwres_vs_ipred <- function(model, predictions, residuals, stratify_on=NULL, bins=8) {
	bins <- convert_input(bins, "int")
	func_out <- pharmpy$modeling$plot_abs_cwres_vs_ipred(model, predictions, residuals, stratify_on=stratify_on, bins=bins)
	return(py_to_r(func_out))
}

#' @title
#' plot_cwres_vs_idv
#' 
#' @description
#' Plot CWRES vs idv
#' 
#' @param model (Model) Pharmpy model
#' @param residuals (data.frame) DataFrame containing CWRES
#' @param stratify_on (str) Name of parameter for stratification
#' @param bins (numeric) Number of bins for stratification
#'  
#' @return (alt.Chart) Plot
#' 
#' @examples
#' \dontrun{
#' }
#' 
#' @export
plot_cwres_vs_idv <- function(model, residuals, stratify_on=NULL, bins=8) {
	bins <- convert_input(bins, "int")
	func_out <- pharmpy$modeling$plot_cwres_vs_idv(model, residuals, stratify_on=stratify_on, bins=bins)
	return(py_to_r(func_out))
}

#' @title
#' plot_dv_vs_ipred
#' 
#' @description
#' Plot DV vs IPRED
#' 
#' @param model (Model) Pharmpy model
#' @param predictions (data.frame) DataFrame containing the predictions
#' @param stratify_on (str) Name of parameter for stratification
#' @param bins (numeric) Number of bins for stratification
#'  
#' @return (alt.Chart) Plot
#' 
#' @examples
#' \dontrun{
#' }
#' 
#' @export
plot_dv_vs_ipred <- function(model, predictions, stratify_on=NULL, bins=8) {
	bins <- convert_input(bins, "int")
	func_out <- pharmpy$modeling$plot_dv_vs_ipred(model, predictions, stratify_on=stratify_on, bins=bins)
	return(py_to_r(func_out))
}

#' @title
#' plot_dv_vs_pred
#' 
#' @description
#' Plot DV vs PRED
#' 
#' @param model (Model) Pharmpy model
#' @param predictions (data.frame) DataFrame containing the predictions
#' @param stratify_on (str) Name of parameter for stratification
#' @param bins (numeric) Number of bins for stratification
#'  
#' @return (alt.Chart) Plot
#' 
#' @examples
#' \dontrun{
#' }
#' 
#' @export
plot_dv_vs_pred <- function(model, predictions, stratify_on=NULL, bins=8) {
	bins <- convert_input(bins, "int")
	func_out <- pharmpy$modeling$plot_dv_vs_pred(model, predictions, stratify_on=stratify_on, bins=bins)
	return(py_to_r(func_out))
}

#' @title
#' plot_eta_distributions
#' 
#' @description
#' Plot eta distributions for all etas
#' 
#' @param model (Model) Previously run Pharmpy model.
#' @param individual_estimates (data.frame) Individual estimates for etas
#'  
#' @return (alt.Chart) Plot
#' 
#' @examples
#' \dontrun{
#' }
#' 
#' @export
plot_eta_distributions <- function(model, individual_estimates) {
	func_out <- pharmpy$modeling$plot_eta_distributions(model, individual_estimates)
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
#' @param individuals (array(numeric) (optional)) A vector of individuals to include. NULL for all individuals
#'  
#' @return (alt.Chart) Plot
#' 
#' @examples
#' \dontrun{
#' }
#' 
#' @export
plot_individual_predictions <- function(model, predictions, individuals=NULL) {
	individuals <- convert_input(individuals, "list")
	func_out <- pharmpy$modeling$plot_individual_predictions(model, predictions, individuals=individuals)
	return(py_to_r(func_out))
}

#' @title
#' plot_iofv_vs_iofv
#' 
#' @description
#' Plot individual OFV of two models against each other
#' 
#' @param iofv1 (array) Estimated iOFV of the first model
#' @param iofv2 (array) Estimated iOFV of the second model
#' @param name1 (str) Name of first model
#' @param name2 (str) Name of second model
#'  
#' @return (alt.Chart) Scatterplot
#' 
#' @examples
#' \dontrun{
#' }
#' 
#' @export
plot_iofv_vs_iofv <- function(iofv1, iofv2, name1, name2) {
	iofv1 <- convert_input(iofv1, "pd.Series")
	iofv2 <- convert_input(iofv2, "pd.Series")
	func_out <- pharmpy$modeling$plot_iofv_vs_iofv(iofv1, iofv2, name1, name2)
	return(py_to_r(func_out))
}

#' @title
#' plot_transformed_eta_distributions
#' 
#' @description
#' Plot transformed eta distributions for all transformed etas
#' 
#' @param model (Model) Previously run Pharmpy model.
#' @param parameter_estimates (array or list(str=numeric)) Parameter estimates of model fit
#' @param individual_estimates (data.frame) Individual estimates for etas
#'  
#' @return (alt.Chart) Plot
#' 
#' 
#' @export
plot_transformed_eta_distributions <- function(model, parameter_estimates, individual_estimates) {
	func_out <- pharmpy$modeling$plot_transformed_eta_distributions(model, parameter_estimates, individual_estimates)
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
}

#' @title
#' read_dataset_from_datainfo
#' 
#' @description
#' Read a dataset given a datainfo object or path to a datainfo file
#' 
#' @param datainfo (DataInfo or str) A datainfo object or a path to a datainfo object
#' @param datatype (str (optional)) A string to specify dataset type
#'  
#' @return (data.frame) The dataset
#' 
#' 
#' @export
read_dataset_from_datainfo <- function(datainfo, datatype=NULL) {
	func_out <- pharmpy$modeling$read_dataset_from_datainfo(datainfo, datatype=datatype)
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' read_model
#' 
#' @description
#' Read model from file
#' 
#' @param path (str) Path to model
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
#' read_model_from_string
#' 
#' @description
#' Read model from the model code in a string
#' 
#' @param code (str) Model code to read
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
read_model_from_string <- function(code) {
	func_out <- pharmpy$modeling$read_model_from_string(code)
	return(py_to_r(func_out))
}

#' @title
#' remove_bioavailability
#' 
#' @description
#' Remove bioavailability from the first dose compartment of model.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- remove_bioavailability(model)
#' }
#' @seealso
#' set_bioavailability
#' 
#' 
#' @export
remove_bioavailability <- function(model) {
	func_out <- pharmpy$modeling$remove_bioavailability(model)
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' has_covariate_effect(model, "CL", "WGT")
#' model <- remove_covariate_effect(model, "CL", "WGT")
#' has_covariate_effect(model, "CL", "WGT")
#' }
#' 
#' @export
remove_covariate_effect <- function(model, parameter, covariate) {
	func_out <- pharmpy$modeling$remove_covariate_effect(model, parameter, covariate)
	return(py_to_r(func_out))
}

#' @title
#' remove_derivative
#' 
#' @description
#' Remove a derivative currently being calculcate when running model. Currently, only
#' derivatives with respect to the prediction is supported. Default is to remove all
#' that are present,
#' First order derivates are specied either by single string or single-element tuple.
#' For instance with_respect_to = "ETA_1" or with_respect_to = ("ETA_1",)
#' 
#' Second order derivatives are specified by giving the two independent varibles in a tuple
#' of tuples. For instance with_respect_to ((ETA_1, EPS_1),)
#' 
#' Multiple derivatives can be specified within a tuple. For instance ((ETA_1, EPS_1), "ETA_1")
#' 
#' Currently, only ETAs and EPSILONs are supported
#' 
#' @param model (Model) Pharmpy modeas.
#' @param with_respect_to (array(array(str) or str) or str (optional)) Parameter name(s) to use as independent variables. Default is NULL.
#'  
#' @return (Pharmpy model.) 
#' 
#' 
#' @export
remove_derivative <- function(model, with_respect_to=NULL) {
	func_out <- pharmpy$modeling$remove_derivative(model, with_respect_to=with_respect_to)
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements$find_assignment("Y")
#' model <- remove_error_model(model)
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
#' @param idx (numeric) index of estimation step to remove (starting from 0)
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- remove_estimation_step(model, 0)
#' ests <- model$execution_steps
#' length(ests)
#' }
#' @seealso
#' add_estimation_step
#' 
#' set_estimation_step
#' 
#' append_estimation_step_options
#' 
#' add_parameter_uncertainty_step
#' 
#' remove_parameter_uncertainty_step
#' 
#' set_evaluation_step
#' 
#' 
#' @export
remove_estimation_step <- function(model, idx) {
	idx <- convert_input(idx, "int")
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
#' @param to_remove (array(str) or str (optional)) Name/names of etas and/or name/names of individual parameters to remove.
#' If NULL, all etas that are IIVs will be removed. NULL is default.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- remove_iiv(model)
#' model$statements$find_assignment("CL")
#' model <- load_example_model("pheno")
#' model <- remove_iiv(model, "V")
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
	to_remove <- convert_input(to_remove, "list")
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
#' @param to_remove (array(str) or str (optional)) Name/names of IOV etas to remove, e.g. 'ETA_IOV_1_1'.
#' If NULL, all etas that are IOVs will be removed. NULL is default.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- remove_iov(model)
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
	to_remove <- convert_input(to_remove, "list")
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- remove_lag_time(model)
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
#' Does nothing if none of the limits are specified.
#' 
#' @param model (Model) Pharmpy model object
#' @param lloq (numeric or str (optional)) Value or column name for lower limit of quantification.
#' @param uloq (numeric or str (optional)) Value or column name for upper limit of quantification.
#' @param blq (str (optional)) Column name for below limit of quantification indicator.
#' @param alq (str (optional)) Column name for above limit of quantification indicator.
#' @param keep (numeric (optional)) Number of loq records to keep for each run of consecutive loq records.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- remove_loq_data(model, lloq=10, uloq=40)
#' length(model$dataset)
#' }
#' @seealso
#' set_lloq_data
#' 
#' transform_blq
#' 
#' 
#' @export
remove_loq_data <- function(model, lloq=NULL, uloq=NULL, blq=NULL, alq=NULL, keep=0) {
	keep <- convert_input(keep, "int")
	func_out <- pharmpy$modeling$remove_loq_data(model, lloq=lloq, uloq=uloq, blq=blq, alq=alq, keep=keep)
	return(py_to_r(func_out))
}

#' @title
#' remove_parameter_uncertainty_step
#' 
#' @description
#' Removes parameter uncertainty step from the final estimation step
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- remove_parameter_uncertainty_step(model)
#' ests <- model$execution_steps
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
#' add_parameter_uncertainty_step
#' 
#' set_evaluation_step
#' 
#' 
#' @export
remove_parameter_uncertainty_step <- function(model) {
	func_out <- pharmpy$modeling$remove_parameter_uncertainty_step(model)
	return(py_to_r(func_out))
}

#' @title
#' remove_peripheral_compartment
#' 
#' @description
#' Remove a peripheral distribution compartment from model
#' 
#' If name is set, a peripheral compartment will be removed from the compartment
#' with the specified name.
#' 
#' Initial estimates:
#' 
#' ==  ===================================================
#' n
#' ==  ===================================================
#' 2   (equation could not be rendered, see API doc on website)
#' 3   (equation could not be rendered, see API doc on website)
#' ==  ===================================================
#' 
#' @param model (Model) Pharmpy model
#' @param name (str) Name of compartment to remove peripheral compartment from.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_peripheral_compartments(model, 2)
#' model <- remove_peripheral_compartment(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_peripheral_compartment
#' 
#' add_peripheral_compartment
#' 
#' 
#' @export
remove_peripheral_compartment <- function(model, name=NULL) {
	func_out <- pharmpy$modeling$remove_peripheral_compartment(model, name=name)
	return(py_to_r(func_out))
}

#' @title
#' remove_predictions
#' 
#' @description
#' Remove predictions and/or residuals
#' 
#' Remove predictions from estimation step.
#' 
#' @param model (Model) Pharmpy model
#' @param to_remove (array(str)) List of predictions to remove
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- remove_predictions(model, 'all')
#' model$execution_steps[-1].predictions
#' }
#' @seealso
#' add_predictions
#' 
#' add_residuals
#' 
#' set_estimation_step
#' 
#' add_estimation_step
#' 
#' remove_estimation_step
#' 
#' append_estimation_step_options
#' 
#' add_parameter_uncertainty_step
#' 
#' remove_parameter_uncertainty_step
#' 
#' 
#' @export
remove_predictions <- function(model, to_remove='all') {
	to_remove <- convert_input(to_remove, "list")
	func_out <- pharmpy$modeling$remove_predictions(model, to_remove=to_remove)
	return(py_to_r(func_out))
}

#' @title
#' remove_residuals
#' 
#' @description
#' Remove predictions and/or residuals
#' 
#' Remove residuals from estimation step.
#' 
#' @param model (Model) Pharmpy model
#' @param to_remove (array(str)) List of predictions to remove
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- remove_residuals(model, 'all')
#' model$execution_steps[-1].residuals
#' }
#' @seealso
#' add_predictions
#' 
#' add_residuals
#' 
#' set_estimation_step
#' 
#' add_estimation_step
#' 
#' remove_estimation_step
#' 
#' append_estimation_step_options
#' 
#' add_parameter_uncertainty_step
#' 
#' remove_parameter_uncertainty_step
#' 
#' 
#' @export
remove_residuals <- function(model, to_remove=NULL) {
	to_remove <- convert_input(to_remove, "list")
	func_out <- pharmpy$modeling$remove_residuals(model, to_remove=to_remove)
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
#' @return (Model) Pharmpy model object
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
#' @param new_names (list(str or Expr=str or Expr)) From old name or symbol to new name or symbol
#'  
#' @return (Model) Pharmpy model object
#' 
#' 
#' @export
rename_symbols <- function(model, new_names) {
	func_out <- pharmpy$modeling$rename_symbols(model, new_names)
	return(py_to_r(func_out))
}

#' @title
#' replace_non_random_rvs
#' 
#' @description
#' Replace all random variables that are not actually random
#' 
#' Some random variables are constant. For example a normal
#' distribution with the variance parameter fixed to 0 will always
#' yield a single value when sampled. This function will find all such
#' random variables and replace them with their constant value in the model.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) A new model
#' 
#' 
#' @export
replace_non_random_rvs <- function(model) {
	func_out <- pharmpy$modeling$replace_non_random_rvs(model)
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
#' @param resamples (numeric) Number of resamples (iterations) to make
#' @param stratify (str (optional)) Name of column to use for stratification.
#' The values in the stratification column must be equal within a group so that the group
#' can be uniquely determined. A ValueError exception will be raised otherwise.
#' @param sample_size (numeric (optional)) The number of groups that should be sampled. The default is
#' the number of groups. If using stratification the default is to sample using the
#' proportion of the strata in the dataset. A list of specific sample sizes
#' for each stratum can also be supplied.
#' @param replace (logical) A boolean controlling whether sampling should be done with or
#' without replacement
#' @param name_pattern (str) Name to use for generated datasets. A number starting from 1 will
#' be put in the placeholder.
#' @param name (str (optional)) Option to name pattern in case of only one resample
#'  
#' @return (iterator) An iterator yielding tuples of a resampled DataFrame and a vector of resampled groups in order
#' 
#' 
#' @export
resample_data <- function(dataset_or_model, group, resamples=1, stratify=NULL, sample_size=NULL, replace=FALSE, name_pattern='resample_{}', name=NULL) {
	resamples <- convert_input(resamples, "int")
	sample_size <- convert_input(sample_size, "int")
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
#' @param parameters (array(str) (optional)) A vector of a subset of individual parameters to sample. Default is NULL, which means all.
#' @param samples_per_id (numeric) Number of samples per individual
#' @param seed (numeric (optional)) Random number generator or seed
#'  
#' @return (data.frame) Pool of samples in a DataFrame
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' rng <- create_rng(23)
#' ie <- results$individual_estimates
#' iec <- results$individual_estimates_covariance
#' sample_individual_estimates(model, ie, iec, samples_per_id=2, seed=rng)
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
sample_individual_estimates <- function(model, individual_estimates, individual_estimates_covariance, parameters=NULL, samples_per_id=100, seed=NULL) {
	parameters <- convert_input(parameters, "list")
	samples_per_id <- convert_input(samples_per_id, "int")
	seed <- convert_input(seed, "int")
	func_out <- pharmpy$modeling$sample_individual_estimates(model, individual_estimates, individual_estimates_covariance, parameters=parameters, samples_per_id=samples_per_id, seed=seed)
	func_out <- reset_index_df(func_out)
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
#' @param parameter_estimates (array) Parameter estimates to use as means in sampling
#' @param covariance_matrix (data.frame) Parameter uncertainty covariance matrix
#' @param force_posdef_samples (numeric (optional)) Set to how many iterations to do before forcing all samples to be positive definite. NULL is
#' default and means never and 0 means always
#' @param force_posdef_covmatrix (logical) Set to TRUE to force the input covariance matrix to be positive definite
#' @param n (numeric) Number of samples
#' @param seed (numeric (optional)) Random number generator
#'  
#' @return (data.frame) A dataframe with one sample per row
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' rng <- create_rng(23)
#' cov <- results$covariance_matrix
#' pe <- results$parameter_estimates
#' sample_parameters_from_covariance_matrix(model, pe, cov, n=3, seed=rng)
#' }
#' @seealso
#' sample_parameters_uniformly : Sample parameter vectors using uniform distribution
#' 
#' sample_individual_estimates : Sample individual estiates given their covariance
#' 
#' 
#' @export
sample_parameters_from_covariance_matrix <- function(model, parameter_estimates, covariance_matrix, force_posdef_samples=NULL, force_posdef_covmatrix=FALSE, n=1, seed=NULL) {
	parameter_estimates <- convert_input(parameter_estimates, "pd.Series")
	force_posdef_samples <- convert_input(force_posdef_samples, "int")
	n <- convert_input(n, "int")
	seed <- convert_input(seed, "int")
	func_out <- pharmpy$modeling$sample_parameters_from_covariance_matrix(model, parameter_estimates, covariance_matrix, force_posdef_samples=force_posdef_samples, force_posdef_covmatrix=force_posdef_covmatrix, n=n, seed=seed)
	func_out <- reset_index_df(func_out)
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
#' @param parameter_estimates (array) Parameter estimates for parameters to use
#' @param fraction (numeric) Fraction of estimate value to use for distribution bounds
#' @param force_posdef_samples (numeric (optional)) Number of samples to reject before forcing variability parameters to give
#' positive definite covariance matrices.
#' @param n (numeric) Number of samples
#' @param seed (numeric (optional)) Random number generator or seed
#' @param scale (str) Scale to perform sampling on. Valid options are 'normal' and 'UCP'
#'  
#' @return (data.frame) samples
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' rng <- create_rng(23)
#' pe <- results$parameter_estimates
#' sample_parameters_uniformly(model, pe, n=3, seed=rng)
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
sample_parameters_uniformly <- function(model, parameter_estimates, fraction=0.1, force_posdef_samples=NULL, n=1, seed=NULL, scale='normal') {
	parameter_estimates <- convert_input(parameter_estimates, "pd.Series")
	force_posdef_samples <- convert_input(force_posdef_samples, "int")
	n <- convert_input(n, "int")
	seed <- convert_input(seed, "int")
	func_out <- pharmpy$modeling$sample_parameters_uniformly(model, parameter_estimates, fraction=fraction, force_posdef_samples=force_posdef_samples, n=n, seed=seed, scale=scale)
	func_out <- reset_index_df(func_out)
	return(py_to_r(func_out))
}

#' @title
#' set_additive_error_model
#' 
#' @description
#' Set an additive error model. Initial estimate for new sigma is (equation could not be rendered, see API doc on website)
#' 
#' The error function being applied depends on the data transformation. The table displays
#' some examples.
#' 
#' +------------------------+----------------------------------------+
#' | Data transformation    | Additive error                         |
#' +========================+========================================+
#' | (equation could not be rendered, see API doc on website)
#' +------------------------+----------------------------------------+
#' | (equation could not be rendered, see API doc on website)
#' +------------------------+----------------------------------------+
#' 
#' @param model (Model) Set error model for this model
#' @param dv (str or Expr or numeric (optional)) Name or DVID of dependent variable. NULL for the default (first or only)
#' @param data_trans (numeric or str or Expr (optional)) A data transformation expression or NULL (default) to use the transformation
#' specified by the model. Series expansion will be used for approximation.
#' @param series_terms (numeric) Number of terms to use for the series expansion approximation for data
#' transformation.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements$find_assignment("Y")
#' model <- set_additive_error_model(model)
#' model$statements$find_assignment("Y")
#' model <- load_example_model("pheno")
#' model$statements$find_assignment("Y")
#' model <- set_additive_error_model(model, data_trans="log(Y)")
#' model$statements$find_assignment("Y")
#' }
#' @seealso
#' set_proportional_error_model : Proportional error model
#' 
#' set_combined_error_model : Combined error model
#' 
#' 
#' @export
set_additive_error_model <- function(model, dv=NULL, data_trans=NULL, series_terms=2) {
	series_terms <- convert_input(series_terms, "int")
	func_out <- pharmpy$modeling$set_additive_error_model(model, dv=dv, data_trans=data_trans, series_terms=series_terms)
	return(py_to_r(func_out))
}

#' @title
#' set_baseline_effect
#' 
#' @description
#' Create baseline effect model.
#' 
#' Currently implemented baseline effects are:
#' 
#' Constant baseline effect (const):
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' @param model (Model) Pharmpy model
#' @param expr (str) Name of baseline effect function.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_baseline_effect(model, expr='const')
#' model$statements$find_assignment("E")
#' }
#' 
#' @export
set_baseline_effect <- function(model, expr='const') {
	func_out <- pharmpy$modeling$set_baseline_effect(model, expr=expr)
	return(py_to_r(func_out))
}

#' @title
#' set_combined_error_model
#' 
#' @description
#' Set a combined error model. Initial estimates for new sigmas are (equation could not be rendered, see API doc on website)
#' proportional and 0.09 for additive.
#' 
#' The error function being applied depends on the data transformation.
#' 
#' +------------------------+-----------------------------------------------------+
#' | Data transformation    | Combined error                                      |
#' +========================+=====================================================+
#' | (equation could not be rendered, see API doc on website)
#' +------------------------+-----------------------------------------------------+
#' | (equation could not be rendered, see API doc on website)
#' +------------------------+-----------------------------------------------------+
#' 
#' @param model (Model) Set error model for this model
#' @param dv (str or Expr or numeric (optional)) Name or DVID of dependent variable. NULL for the default (first or only)
#' @param data_trans (numeric or str or Expr (optional)) A data transformation expression or NULL (default) to use the transformation
#' specified by the model.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- remove_error_model(load_example_model("pheno"))
#' model <- set_combined_error_model(model)
#' model$statements$find_assignment("Y")
#' model <- remove_error_model(load_example_model("pheno"))
#' model <- set_combined_error_model(model, data_trans="log(Y)")
#' model$statements$find_assignment("Y")
#' }
#' @seealso
#' set_additive_error_model : Additive error model
#' 
#' set_proportional_error_model: Proportional error model
#' 
#' 
#' @export
set_combined_error_model <- function(model, dv=NULL, data_trans=NULL) {
	func_out <- pharmpy$modeling$set_combined_error_model(model, dv=dv, data_trans=data_trans)
	return(py_to_r(func_out))
}

#' @title
#' set_covariates
#' 
#' @description
#' Set columns in the dataset to be covariates in the datainfo
#' 
#' @param model (Model) Pharmpy model
#' @param covariates (array(str)) List of column names
#'  
#' @return (Model) Pharmpy model object
#' 
#' 
#' @export
set_covariates <- function(model, covariates) {
	covariates <- convert_input(covariates, "list")
	func_out <- pharmpy$modeling$set_covariates(model, covariates)
	return(py_to_r(func_out))
}

#' @title
#' set_dataset
#' 
#' @description
#' Load the dataset given datainfo
#' 
#' @param model (Model) Pharmpy model
#' @param path_or_df (str or data.frame) Dataset path or dataframe
#' @param datatype (str (optional)) Type of dataset (optional)
#'  
#' @return (Model) Pharmpy model with new dataset and updated datainfo
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- unload_dataset(model)
#' dataset_path <- model$datainfo$path
#' model$dataset is NULL
#' model <- set_dataset(model, dataset_path, datatype='nonmem')
#' model$dataset
#' }
#' 
#' @export
set_dataset <- function(model, path_or_df, datatype=NULL) {
	func_out <- pharmpy$modeling$set_dataset(model, path_or_df, datatype=datatype)
	return(py_to_r(func_out))
}

#' @title
#' set_direct_effect
#' 
#' @description
#' Add an effect to a model.
#' 
#' Implemented PD models are:
#' 
#' 
#' * Linear:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Emax:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Step effect:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Sigmoidal:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' * Log-linear:
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' (equation could not be rendered, see API doc on website)
#' 
#' @param model (Model) Pharmpy model
#' @param expr (str) Name of PD effect function.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_direct_effect(model, "linear")
#' model$statements$find_assignment("E")
#' }
#' 
#' @export
set_direct_effect <- function(model, expr) {
	func_out <- pharmpy$modeling$set_direct_effect(model, expr)
	return(py_to_r(func_out))
}

#' @title
#' set_dtbs_error_model
#' 
#' @description
#' Dynamic transform both sides
#' 
#' @param model (Model) Pharmpy model
#' @param fix_to_log (logical) Set to TRUE to fix lambda and zeta to 0, i.e. emulating log-transformed data
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_dtbs_error_model(model)
#' }
#' 
#' @export
set_dtbs_error_model <- function(model, fix_to_log=FALSE) {
	func_out <- pharmpy$modeling$set_dtbs_error_model(model, fix_to_log=fix_to_log)
	return(py_to_r(func_out))
}

#' @title
#' set_dvid
#' 
#' @description
#' Set a column to act as DVID. Replace DVID if one is already set.
#' 
#' @param model (Model) Pharmpy model
#' @param name (str) Name of DVID column
#'  
#' @return (Model) Pharmpy model object
#' 
#' 
#' @export
set_dvid <- function(model, name) {
	func_out <- pharmpy$modeling$set_dvid(model, name)
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
#' @param idx (numeric) index of estimation step, default is 0 (first estimation step)
#' @param ... Arguments to pass to EstimationStep (such as interaction, evaluation)
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' opts <- list('NITER'=1000, 'ISAMPLE'=100)
#' model <- set_estimation_step(model, 'IMP', evaluation=TRUE, tool_options=opts)
#' model$execution_steps[1]
#' }
#' @seealso
#' add_estimation_step
#' 
#' remove_estimation_step
#' 
#' append_estimation_step_options
#' 
#' add_parameter_uncertainty_step
#' 
#' remove_parameter_uncertainty_step
#' 
#' set_evaluation_step
#' 
#' 
#' @export
set_estimation_step <- function(model, method, idx=0, ...) {
	idx <- convert_input(idx, "int")
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
#' @param idx (numeric) Index of estimation step, default is -1 (last estimation step)
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_evaluation_step(model)
#' model$execution_steps[1]
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
#' add_parameter_uncertainty_step
#' 
#' remove_parameter_uncertainty_step
#' 
#' 
#' @export
set_evaluation_step <- function(model, idx=-1) {
	idx <- convert_input(idx, "int")
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
#' If multiple doses is set to the affected compartment, currently only iv+oral
#' doses (one of each) is supported
#' 
#' @param model (Model) Model to set or change to use first order absorption rate
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_first_order_absorption(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_instantaneous_absorption
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_first_order_elimination(model)
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
#' @param dv (str or Expr or numeric (optional)) Name/names of epsilons to multiply with exponential etas. If NULL, all epsilons will
#' be chosen. NULL is default.
#' @param list_of_eps (array(str) or str (optional)) Boolean of whether all RUVs from input should use the same new ETA or if one ETA
#' should be created for each RUV. TRUE is default.
#' @param same_eta (logical) Custom names of new etas. Must be equal to the number epsilons or 1 if same eta.
#' @param eta_names (array(str) or str (optional)) Name or DVID of dependent variable. NULL for the default (first or only)
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_iiv_on_ruv(model)
#' model$statements$find_assignment("Y")
#' }
#' @seealso
#' set_power_on_ruv
#' 
#' 
#' @export
set_iiv_on_ruv <- function(model, dv=NULL, list_of_eps=NULL, same_eta=TRUE, eta_names=NULL) {
	list_of_eps <- convert_input(list_of_eps, "list")
	eta_names <- convert_input(eta_names, "list")
	func_out <- pharmpy$modeling$set_iiv_on_ruv(model, dv=dv, list_of_eps=list_of_eps, same_eta=same_eta, eta_names=eta_names)
	return(py_to_r(func_out))
}

#' @title
#' set_initial_condition
#' 
#' @description
#' Set an initial condition for the ode system
#' 
#' If the initial condition is already set it will be updated. If the initial condition
#' is set to zero at time zero it will be removed (since the default is 0).
#' 
#' @param model (Model) Pharmpy model
#' @param compartment (str) Name of the compartment
#' @param expression (numeric or str or Expr) The expression of the initial condition
#' @param time (numeric or str or Expr) Time point. Default 0
#'  
#' @return (model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_initial_condition(model, "CENTRAL", 10)
#' get_initial_conditions(model)
#' }
#' 
#' @export
set_initial_condition <- function(model, compartment, expression, time=0) {
	func_out <- pharmpy$modeling$set_initial_condition(model, compartment, expression, time=time)
	return(py_to_r(func_out))
}

#' @title
#' set_initial_estimates
#' 
#' @description
#' Update initial parameter estimate for a model
#' 
#' Updates initial estimates of population parameters for a model.
#' If the new initial estimates are out of bounds or NaN this function will raise.
#' 
#' @param model (Model) Pharmpy model to update initial estimates
#' @param inits (list(str=numeric)) Initial parameter estimates to update
#' @param move_est_close_to_bounds (logical) Move estimates that are close to bounds. If correlation >0.99 the correlation will
#' be set to 0.9, if variance is <0.001 the variance will be set to 0.01.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' model$parameters$inits
#' model <- set_initial_estimates(model, results$parameter_estimates)
#' model$parameters$inits
#' model <- load_example_model("pheno")
#' model <- set_initial_estimates(model, {'PTVCL': 2.0})
#' model$parameters['PTVCL']
#' }
#' @seealso
#' fix_parameters_to : Fixing and setting parameter initial estimates in the same function
#' 
#' unfix_paramaters_to : Unfixing parameters and setting a new initial estimate in the same
#' 
#' 
#' @export
set_initial_estimates <- function(model, inits, move_est_close_to_bounds=FALSE) {
	inits <- convert_input(inits, "Mapping")
	func_out <- pharmpy$modeling$set_initial_estimates(model, inits, move_est_close_to_bounds=move_est_close_to_bounds)
	return(py_to_r(func_out))
}

#' @title
#' set_instantaneous_absorption
#' 
#' @description
#' Set or change to instantaneous absorption rate.
#' 
#' Currently lagtime together with instantaneous absorption is not supported.
#' 
#' @param model (Model) Model to set or change absorption rate
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_instantaneous_absorption(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_zero_order_absorption
#' 
#' set_first_order_absorption
#' 
#' 
#' @export
set_instantaneous_absorption <- function(model) {
	func_out <- pharmpy$modeling$set_instantaneous_absorption(model)
	return(py_to_r(func_out))
}

#' @title
#' set_lloq_data
#' 
#' @description
#' Set a dv value for lloq data records
#' 
#' @param model (Model) Pharmpy model object
#' @param value (str or numeric or Expr) The new dv value
#' @param lloq (numeric or str (optional)) Value or column name for lower limit of quantification.
#' @param blq (str (optional)) Column name for below limit of quantification indicator.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_lloq_data(model, 0, lloq=10)
#' }
#' @seealso
#' remove_loq_data
#' 
#' transform_blq
#' 
#' 
#' @export
set_lloq_data <- function(model, value, lloq=NULL, blq=NULL) {
	func_out <- pharmpy$modeling$set_lloq_data(model, value, lloq=lloq, blq=blq)
	return(py_to_r(func_out))
}

#' @title
#' set_lower_bounds
#' 
#' @description
#' Set parameter lower bounds
#' 
#' @param model (Model) Pharmpy model
#' @param bounds (list(str=numeric)) A list of parameter bounds for parameters to change
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_lower_bounds(model, {'PTVCL': -10})
#' model$parameters['PTVCL']
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
#' Initial estimate for CLMM is set to CL and KM is set to (equation could not be rendered, see API doc on website)
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_michaelis_menten_elimination(model)
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
#' Initial estimate for CLMM is set to CL/2 and KM is set to (equation could not be rendered, see API doc on website)
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_mixed_mm_fo_elimination(model)
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$name
#' model <- set_name(model, "run2")
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_ode_solver(model, 'LSODA')
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
#' Sets the number of peripheral compartments for central compartment to a specified number.
#' 
#' If name is set, the peripheral compartment will be added to the compartment
#' with the specified name instead.
#' 
#' @param model (Model) Pharmpy model
#' @param n (numeric) Number of transit compartments
#' @param name (str) Name of compartment to add peripheral to.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_peripheral_compartments(model, 2)
#' model$statements$ode_system
#' }
#' @seealso
#' add_peripheral_compartment
#' 
#' remove_peripheral_compartment
#' 
#' 
#' @export
set_peripheral_compartments <- function(model, n, name=NULL) {
	n <- convert_input(n, "int")
	func_out <- pharmpy$modeling$set_peripheral_compartments(model, n, name=name)
	return(py_to_r(func_out))
}

#' @title
#' set_power_on_ruv
#' 
#' @description
#' Applies a power effect to provided epsilons. If a dependent variable
#' is provided, then only said epsilons affecting said variable will be changed.
#' 
#' Initial estimates for new thetas are 1 if the error
#' model is proportional, otherwise they are 0.1.
#' 
#' NOTE : If no DVs or epsilons are specified, all epsilons with the same name
#' will be connected to the same theta. Running the function per DV will give
#' each epsilon a specific theta.
#' 
#' @param model (Model) Pharmpy model to create block effect on.
#' @param list_of_eps (str or array (optional)) Name/names of epsilons to apply power effect. If NULL, all epsilons will be used.
#' NULL is default.
#' @param dv (str or Expr or numeric (optional)) Name or DVID of dependent variable. NULL will change the epsilon on all occurences
#' regardless of affected dependent variable.
#' @param lower_limit (numeric (optional)) Lower limit of power (theta). NULL for no limit.
#' @param ipred (str or Expr (optional)) Symbol to use as IPRED. Default is to autodetect expression for IPRED.
#' @param zero_protection (logical) Set to TRUE to add code protecting from IPRED=0
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_power_on_ruv(model)
#' model$statements$find_assignment("Y")
#' }
#' @seealso
#' set_iiv_on_ruv
#' 
#' 
#' @export
set_power_on_ruv <- function(model, list_of_eps=NULL, dv=NULL, lower_limit=0.01, ipred=NULL, zero_protection=FALSE) {
	func_out <- pharmpy$modeling$set_power_on_ruv(model, list_of_eps=list_of_eps, dv=dv, lower_limit=lower_limit, ipred=ipred, zero_protection=zero_protection)
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
#' | (equation could not be rendered, see API doc on website)
#' +------------------------+----------------------------------------+
#' | (equation could not be rendered, see API doc on website)
#' +------------------------+----------------------------------------+
#' 
#' @param model (Model) Set error model for this model
#' @param dv (str or Expr or numeric (optional)) Name or DVID of dependent variable. NULL for the default (first or only)
#' @param data_trans (numeric or str or Expr (optional)) A data transformation expression or NULL (default) to use the transformation
#' specified by the model.
#' @param zero_protection (logical) Set to TRUE to add code protecting from IPRED=0
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- remove_error_model(load_example_model("pheno"))
#' model <- set_proportional_error_model(model)
#' model$statements$after_odes
#' model <- remove_error_model(load_example_model("pheno"))
#' model <- set_proportional_error_model(
#'  model,
#'  data_trans="log(Y)"
#' model$statements$after_odes
#' }
#' @seealso
#' set_additive_error_model : Additive error model
#' 
#' set_combined_error_model : Combined error model
#' 
#' 
#' @export
set_proportional_error_model <- function(model, dv=NULL, data_trans=NULL, zero_protection=TRUE) {
	func_out <- pharmpy$modeling$set_proportional_error_model(model, dv=dv, data_trans=data_trans, zero_protection=zero_protection)
	return(py_to_r(func_out))
}

#' @title
#' set_reference_values
#' 
#' @description
#' Set reference values for selected columns
#' 
#' All values for each selected column will be replaced. For dose columns
#' only the values for dosing events will be replaced.
#' 
#' @param model (Model) Pharmpy model object
#' @param refs (list(str=numeric)) Pairs of column names and reference values
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_reference_values(model, {'WGT': 0.5, 'AMT': 4.0})
#' model$dataset
#' }
#' 
#' @export
set_reference_values <- function(model, refs) {
	func_out <- pharmpy$modeling$set_reference_values(model, refs)
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_seq_zo_fo_absorption(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_instantaneous_absorption
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
#' set_simulation
#' 
#' @description
#' Change model into simulation model
#' 
#' @param model (Model) Pharmpy model
#' @param n (numeric) Number of replicates
#' @param seed (numeric) Random seed for the simulation
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_simulation(model, n=10, seed=1234)
#' steps <- model$execution_steps
#' steps[1]
#' }
#' 
#' @export
set_simulation <- function(model, n=1, seed=64206) {
	n <- convert_input(n, "int")
	seed <- convert_input(seed, "int")
	func_out <- pharmpy$modeling$set_simulation(model, n=n, seed=seed)
	return(py_to_r(func_out))
}

#' @title
#' set_time_varying_error_model
#' 
#' @description
#' Set a time varying error model per time cutoff
#' 
#' @param model (Model) Pharmpy model
#' @param cutoff (numeric) A cutoff value for idv column
#' @param idv (str) Time or time after dose, default is Time
#' @param dv (str or Expr or numeric (optional)) Name or DVID of dependent variable. NULL for the default (first or only)
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_time_varying_error_model(model, cutoff=1.0)
#' model$statements$find_assignment("Y")
#' }
#' 
#' @export
set_time_varying_error_model <- function(model, cutoff, idv='TIME', dv=NULL) {
	func_out <- pharmpy$modeling$set_time_varying_error_model(model, cutoff, idv=idv, dv=dv)
	return(py_to_r(func_out))
}

#' @title
#' set_tmdd
#' 
#' @description
#' Sets target mediated drug disposition
#' 
#' Implemented target mediated drug disposition (TMDD) models are:
#' 
#' * Full model
#' * Irreversible binding approximation (IB)
#' * Constant total receptor approximation (CR)
#' * Irreversible binding and constant total receptor approximation (CR+IB)
#' * Quasi steady-state approximation (QSS)
#' * Wagner
#' * Michaelis-Menten approximation (MMAPP)
#' 
#' 
#' @param model (Model) Pharmpy model
#' @param type (str) Type of TMDD model
#' @param dv_types (list(str=numeric) (optional)) Dictionary of DV types for TMDD models with multiple DVs (e.g. dv_types = {'drug' : 1, 'target': 2}).
#' Default is NULL which means that all observations are treated as drug observations.
#' For dv = 1 the only allowed keys are 'drug' and 'drug_tot'. If no DV for drug is specified then (free) drug
#' will have dv = 1.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_tmdd(model, "full")
#' }
#' 
#' @export
set_tmdd <- function(model, type, dv_types=NULL) {
	func_out <- pharmpy$modeling$set_tmdd(model, type, dv_types=dv_types)
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
#' @param n (numeric) Number of transit compartments
#' @param keep_depot (logical) FALSE to convert depot compartment into a transit compartment
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_transit_compartments(model, 3)
#' model$statements$ode_system
#' }
#' @seealso
#' add_lag_time
#' 
#' 
#' @export
set_transit_compartments <- function(model, n, keep_depot=TRUE) {
	n <- convert_input(n, "int")
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
#' @param bounds (list(str=numeric)) A list of parameter bounds for parameters to change
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_upper_bounds(model, list('PTVCL'=10))
#' model$parameters['PTVCL']
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_weighted_error_model(model)
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_zero_order_absorption(model)
#' model$statements$ode_system
#' }
#' @seealso
#' set_instantaneous_absorption
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_zero_order_elimination(model)
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
#' set_zero_order_input
#' 
#' @description
#' Set a zero order input for the ode system
#' 
#' If the zero order input is already set it will be updated.
#' 
#' @param model (Model) Pharmpy model
#' @param compartment (str) Name of the compartment
#' @param expression (numeric or str or Expr) The expression of the zero order input
#'  
#' @return (model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_zero_order_input(model, "CENTRAL", 10)
#' get_zero_order_inputs(model)
#' }
#' 
#' @export
set_zero_order_input <- function(model, compartment, expression) {
	func_out <- pharmpy$modeling$set_zero_order_input(model, compartment, expression)
	return(py_to_r(func_out))
}

#' @title
#' simplify_expression
#' 
#' @description
#' Simplify expression given constraints in model
#' 
#' @param model (Model) Pharmpy model object
#' @param expr (str or numeric or Expr) Expression to simplify
#'  
#' @return (Expression) Simplified expression
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' simplify_expression(model, "Abs(PTVCL)")
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$statements$ode_system
#' model <- solve_ode_system(model)
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
#' @param rvs (array(str) or str (optional)) Name/names of etas to separate. If NULL, all etas that are IIVs and
#' non-fixed will become single. NULL is default.
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- create_joint_distribution(model, c('ETA_1', 'ETA_2'))
#' model$random_variables$etas
#' model <- split_joint_distribution(model, c('ETA_1', 'ETA_2'))
#' model$random_variables$etas
#' }
#' @seealso
#' create_joint_distribution : combine etas into a join distribution
#' 
#' 
#' @export
split_joint_distribution <- function(model, rvs=NULL) {
	rvs <- convert_input(rvs, "list")
	func_out <- pharmpy$modeling$split_joint_distribution(model, rvs=rvs)
	return(py_to_r(func_out))
}

#' @title
#' transform_blq
#' 
#' @description
#' Transform for BLQ data
#' 
#' Transform a given model, methods available are m1, m3, m4, m5, m6 and m7 (1).
#' The blq information can come from the dataset, the lloq option or a combination. Both LLOQ and BLQ
#' columns are supported. The table below explains which columns are used for the various cases:
#' 
#' +-------------+-------------+------------+-------------------+---------------+-------------------+
#' | lloq option | LLOQ column | BLQ column | Used as indicator | Used as level | Note              |
#' +=============+=============+============+===================+===============+===================+
#' | Available   | NA          | NA         | DV < lloq         | lloq          |                   |
#' +-------------+-------------+------------+-------------------+---------------+-------------------+
#' | NA          | Available   | NA         | DV < LLOQ         | LLOQ          |                   |
#' +-------------+-------------+------------+-------------------+---------------+-------------------+
#' | NA          | NA          | Available  | BLQ               | nothing       | Only for M1 and M7|
#' +-------------+-------------+------------+-------------------+---------------+-------------------+
#' | NA          | NA          | NA         | NA                | NA            | No BLQ handling   |
#' +-------------+-------------+------------+-------------------+---------------+-------------------+
#' | NA          | Available   | Available  | BLQ               | LLOQ          | DV column not used|
#' +-------------+-------------+------------+-------------------+---------------+-------------------+
#' | Available   | NA          | Available  | BLQ               | lloq          |                   |
#' +-------------+-------------+------------+-------------------+---------------+-------------------+
#' | Available   | Available   | NA         | DV < lloq         | lloq          | Column overridden |
#' +-------------+-------------+------------+-------------------+---------------+-------------------+
#' | Available   | Available   | Available  | DV < lloq         | lloq          | Columns overridden|
#' +-------------+-------------+------------+-------------------+---------------+-------------------+
#' 
#' BLQ observations are defined as shown in the table above.
#' If both a BLQ and an LLOQ column exist in the dataset and no lloq is specified then all dv values in
#' rows with BLQ = 1 are counted as BLQ observations. If instead an lloq value is specified then all rows with
#' dv values below the lloq value are counted as BLQ observations.
#' If no lloq is specified and no BLQ column exists in the dataset then all rows with dv values below the value
#' specified in the DV column are counted as BLQ observations.
#' 
#' 
#' M1 method:
#' All BLQ observations are discarded.
#' This may affect the size of the dataset.
#' M3 method:
#' Including the probability that the BLQ observations are below the LLOQ
#' as part of the maximum likelihood estimation.
#' For more details see :ref:`(1)<ref_article>`.
#' This method modifies the Y statement of the model (see examples below).
#' M4 method:
#' Including the probability that the BLQ observations are below the LLOQ and positive
#' as part of the maximum likelihood estimation.
#' For more details see :ref:`(1)<ref_article>`.
#' This method modifies the Y statement of the model (see examples below).
#' M5 method:
#' All BLQ observations are replaced by level/2, where level = lloq if lloq is specified.
#' Else level = value specified in LLOQ column (see table above).
#' This method may change entries in the dataset.
#' M6 method:
#' Every BLQ observation in a consecutive series of BLQ observations is discarded except for the first one.
#' The remaining BLQ observations are replaced by level/2, where level = lloq if lloq is specified.
#' Else level = value specified in LLOQ column (see table above).
#' This method may change entries in the dataset as well as the size of the dataset.
#' M7 method:
#' All BLQ observations are replaced by 0.
#' This method may change entries in the dataset.
#' 
#' 
#' 
#' Current limitations of the m3 and m4 method:
#' 
#' * Does not support covariance between epsilons
#' * Supports additive, proportional, combined, and power error model
#' 
#' _ref_article:
#' 
#' (1) Beal SL. Ways to fit a PK model with some data below the quantification
#' limit. J Pharmacokinet Pharmacodyn. 2001 Oct;28(5):481-504. doi: 10.1023/a:1012299115260.
#' Erratum in: J Pharmacokinet Pharmacodyn 2002 Jun;29(3):309. PMID: 11768292.
#' 
#' @param model (Model) Pharmpy model
#' @param method (str) Which BLQ method to use
#' @param lloq (numeric (optional)) LLOQ limit to use, if NULL Pharmpy will use the BLQ/LLOQ column in the dataset
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- transform_blq(model, method='m4', lloq=0.1)
#' model$statements$find_assignment("Y")
#' }
#' @seealso
#' remove_loq_data
#' 
#' set_lloq_data
#' 
#' 
#' @export
transform_blq <- function(model, method='m4', lloq=NULL) {
	func_out <- pharmpy$modeling$transform_blq(model, method=method, lloq=lloq)
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
#' @param list_of_etas (array(str) or str (optional)) Name/names of etas to transform. If NULL, all etas will be transformed (default).
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- transform_etas_boxcox(model, c("ETA_1"))
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
	list_of_etas <- convert_input(list_of_etas, "list")
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
#' @param list_of_etas (array(str) or str (optional)) Name/names of etas to transform. If NULL, all etas will be transformed (default).
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- transform_etas_john_draper(model, c("ETA_1"))
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
	list_of_etas <- convert_input(list_of_etas, "list")
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
#' @param list_of_etas (array(str) or str (optional)) Name/names of etas to transform. If NULL, all etas will be transformed (default).
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- transform_etas_tdist(model, c("ETA_1"))
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
	list_of_etas <- convert_input(list_of_etas, "list")
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
#' @return (Model) Pharmpy model object
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
#' @param parameter_names (array(str)) Remove all constraints for the listed parameters
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$parameters['PTVCL']
#' model <- unconstrain_parameters(model, c('PTVCL'))
#' model$parameters['PTVCL']
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
	parameter_names <- convert_input(parameter_names, "list")
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
#' @param column_names (array(str) or str) List of column names or one column name to undrop
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- drop_columns(model, c('WGT', 'APGR'), mark=TRUE)
#' model <- undrop_columns(model, 'WGT')
#' }
#' @seealso
#' drop_dropped_columns : Drop all columns marked as drop
#' 
#' drop_columns : Drop or mark columns as dropped
#' 
#' 
#' @export
undrop_columns <- function(model, column_names) {
	column_names <- convert_input(column_names, "list")
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
#' @param parameter_names (array(str) or str) one parameter name or a vector of parameter names
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- fix_parameters(model, c('PTVCL', 'PTVV', 'THETA_3'))
#' model$parameters$fix
#' model <- unfix_parameters(model, 'PTVCL')
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
#' @param inits (list(str=numeric)) Inits for all parameters to unfix and change init
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- fix_parameters(model, c('PTVCL', 'PTVV', 'THETA_3'))
#' model$parameters$fix
#' model <- unfix_parameters_to(model, {'PTVCL': 0.5})
#' model$parameters$fix
#' model$parameters['PTVCL']
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
#' unload_dataset
#' 
#' @description
#' Unload the dataset from a model
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (Model) Pharmpy model with dataset removed
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- unload_dataset(model)
#' model$dataset is NULL
#' }
#' 
#' @export
unload_dataset <- function(model) {
	func_out <- pharmpy$modeling$unload_dataset(model)
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
#' @param individual_estimates (array) Individual estimates to use
#' @param force (logical) Set to FALSE to only update if the model had initial individual estimates before
#'  
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' ie <- results$individual_estimates
#' model <- update_initial_individual_estimates(model, ie)
#' }
#' 
#' @export
update_initial_individual_estimates <- function(model, individual_estimates, force=TRUE) {
	individual_estimates <- convert_input(individual_estimates, "pd.Series")
	func_out <- pharmpy$modeling$update_initial_individual_estimates(model, individual_estimates, force=force)
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
#' @return (Model) Pharmpy model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- use_thetas_for_error_stdev(model)
#' model$statements$find_assignment("W")
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
#' vpc_plot
#' 
#' @description
#' Creates a VPC plot for a model
#' 
#' @param model (Model) Pharmpy model
#' @param simulations (data.frame or str) DataFrame containing the simulation data or path to dataset.
#' The dataset has to have one (index) column named "SIM" containing
#' the simulation number, one (index) column named "index" containing the data indices and one dv column.
#' See below for more information.
#' @param binning (str) Binning method. Can be "equal_number" or "equal_width". The default is "equal_number".
#' @param nbins (numeric) Number of bins. Default is 8.
#' @param qi (numeric) Upper quantile. Default is 0.95.
#' @param ci (numeric) Confidence interval. Default is 0.95.
#' @param stratify_on (str (optional)) Parameter to use for stratification. Optional.
#'  
#' @return (alt.Chart) Plot The simulation data should have the following format: +-----+-------+--------+ | SIM | index | DV     | +=====+=======+========+ | 1   | 0     | 0.000  | +-----+-------+--------+ | 1   | 1     | 34.080 | +-----+-------+--------+ | 1   | 2     | 28.858 | +-----+-------+--------+ | 1   | 3     | 0.000  | +-----+-------+--------+ | 1   | 4     | 12.157 | +-----+-------+--------+ | 2   | 0     | 23.834 | +-----+-------+--------+ | 2   | 1     | 0.000  | +-----+-------+--------+ | ... | ...   | ...    | +-----+-------+--------+ | 20  | 2     | 0.000  | +-----+-------+--------+ | 20  | 3     | 31.342 | +-----+-------+--------+ | 20  | 4     | 29.983 | +-----+-------+--------+
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' sim_model <- set_simulation(model, n=100)
#' sim_data <- run_simulation(sim_model)
#' vpc_plot(model, sim_data)
#' }
#' 
#' @export
vpc_plot <- function(model, simulations, binning='equal_number', nbins=8, qi=0.95, ci=0.95, stratify_on=NULL) {
	nbins <- convert_input(nbins, "int")
	func_out <- pharmpy$modeling$vpc_plot(model, simulations, binning=binning, nbins=nbins, qi=qi, ci=ci, stratify_on=stratify_on)
	return(py_to_r(func_out))
}

#' @title
#' write_csv
#' 
#' @description
#' Write dataset to a csv file and updates the datainfo path
#' 
#' @param model (Model) Model whose dataset to write to file
#' @param path (str (optional)) Destination path. Default is to use original path with .csv suffix.
#' @param force (logical) Overwrite file with same path. Default is FALSE.
#'  
#' @return (Model) Updated model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- write_csv(model, path="newdataset$csv")
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
#' @return (Model) Pharmpy model object
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
#' create_report
#' 
#' @description
#' Create standard report for results
#' 
#' The report will be an html created at specified path.
#' 
#' @param results (Results) Results for which to create report
#' @param path (str) Path to report file 
#' 
#' @export
create_report <- function(results, path) {
	tryCatch(
	{
		func_out <- pharmpy$tools$create_report(results, path)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' create_results
#' 
#' @description
#' Create/recalculate results object given path to run directory
#' 
#' @param path (str) Path to run directory
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
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' @param model_or_models (Model or array(Model)) List of models or one single model
#' @param tool (str (optional)) Estimation tool to use. NULL to use default
#' @param path (str (optional)) Path to fit directory
#' @param context (Context (optional)) Run in this context
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
fit <- function(model_or_models, tool=NULL, path=NULL, context=NULL) {
	tryCatch(
	{
		func_out <- pharmpy$tools$fit(model_or_models, tool=tool, path=path, context=context)
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
#' get_model_features
#' 
#' @description
#' Create an MFL representation of an input model
#' 
#' Given an input model. Create a model feature language (MFL) string
#' representation. Can currently extract absorption, elimination, transits,
#' peripherals and lagtime.
#' 
#' @param model (Model) Model to extract features from.
#' @param supress_warnings (logical) Choose to supress warnings if absorption/elimination type cannot be
#' determined. The default is FALSE.
#'  
#' @return (str) A MFL string representation of the input model.
#' 
#' 
#' @export
get_model_features <- function(model, supress_warnings=FALSE) {
	tryCatch(
	{
		func_out <- pharmpy$tools$get_model_features(model, supress_warnings=supress_warnings)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' is_strictness_fulfilled
#' 
#' @description
#' Takes a ModelfitResults object and a statement as input and returns TRUE/FALSE
#' if the evaluation of the statement is TRUE/FALSE.
#' 
#' @param res (ModelfitResults) ModelfitResults object
#' @param model (Model) Model for parameter specific strictness.
#' @param statement (str) A statement containing the strictness criteria
#'  
#' @return (logical) A logical indicating whether the strictness criteria are fulfilled or not.
#' 
#' @examples
#' \dontrun{
#' res <- load_example_modelfit_results('pheno')
#' model <- load_example_model('pheno')
#' is_strictness_fulfilled(res, model, "minimization_successful or rounding_errors")
#' }
#' 
#' @export
is_strictness_fulfilled <- function(res, model, statement) {
	tryCatch(
	{
		func_out <- pharmpy$tools$is_strictness_fulfilled(res, model, statement)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' load_example_modelfit_results
#' 
#' @description
#' Load the modelfit results of an example model
#' 
#' Load the modelfit results of an example model built into Pharmpy
#' 
#' @param name (str) Name of the model. Currently available models are "pheno" and "pheno_linear"
#'  
#' @return (ModelfitResults) Loaded modelfit results object
#' 
#' @examples
#' \dontrun{
#' results <- load_example_modelfit_results("pheno")
#' results$parameter_estimates
#' }
#' 
#' @export
load_example_modelfit_results <- function(name) {
	tryCatch(
	{
		func_out <- pharmpy$tools$load_example_modelfit_results(name)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
		func_out <- reset_index_df(func_out)
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
#' @param outlier_cutoff (numeric) Cutoff threshold for a residual signalling an outlier
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
predict_influential_outliers <- function(model, results, outlier_cutoff=3.0, influential_cutoff=3.84) {
	tryCatch(
	{
		func_out <- pharmpy$tools$predict_influential_outliers(model, results, outlier_cutoff=outlier_cutoff, influential_cutoff=influential_cutoff)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
		func_out <- reset_index_df(func_out)
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
#' results <- load_example_modelfit_results("pheno")
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
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
		func_out <- reset_index_df(func_out)
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
#' @param modelfit_results (ModelfitResults) Pharmpy ModelfitResults object 
#' 
#' @export
print_fit_summary <- function(model, modelfit_results) {
	tryCatch(
	{
		func_out <- pharmpy$tools$print_fit_summary(model, modelfit_results)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' @param base_model_res (ModelfitResults) Results of base model
#' @param models (array(Model)) List of models
#' @param models_res (array(ModelfitResults)) List of modelfit results
#' @param parent_dict (list(str=str) or list(Model=Model) (optional)) Dict where key is child and value is parent. Only relevant for LRT, if NULL base will be set as parent
#' @param strictness (str (optional)) Strictness criteria that are allowed for ranking. Default is "minimization_successful".
#' @param rank_type (str) Name of ranking type. Available options are 'ofv', 'aic', 'bic', 'lrt' (OFV with LRT)
#' @param cutoff (numeric (optional)) Value to use as cutoff. If using LRT, cutoff denotes p-value. Default is NULL
#' @param bic_type (str) Type of BIC to calculate. Default is the mixed effects.
#' @param ... Arguments to pass to calculate BIC (such as `mult_test_p` and `mult_test_p`)
#'  
#' @return (data.frame) DataFrame of the ranked models
#' 
#' @examples
#' \dontrun{
#' model_1 <- load_example_model("pheno")
#' model_2 <- load_example_model("pheno_linear")
#' rank_models(model_1, c(model_2),
#'  rank_type='lrt')
#' }
#' 
#' @export
rank_models <- function(base_model, base_model_res, models, models_res, parent_dict=NULL, strictness='minimization_successful', rank_type='ofv', cutoff=NULL, bic_type='mixed', ...) {
	tryCatch(
	{
		models <- convert_input(models, "list")
		models_res <- convert_input(models_res, "list")
		func_out <- pharmpy$tools$rank_models(base_model, base_model_res, models, models_res, parent_dict=parent_dict, strictness=strictness, rank_type=rank_type, cutoff=cutoff, bic_type=bic_type, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
		func_out <- reset_index_df(func_out)
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
#' @param path (str) Path to model file
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
#' @param path (str) Path to results file
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
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' resume_tool
#' 
#' @description
#' Resume tool workflow from tool database path
#' 
#' @param path (str) The path to the tool database
#'  
#' @return (Results) Results object for tool
#' 
#' @examples
#' \dontrun{
#' res <- resume_tool("resmod_dir1")
#' }
#' 
#' @export
resume_tool <- function(path) {
	tryCatch(
	{
		func_out <- pharmpy$tools$resume_tool(path)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' @param source (str or Context) Source where to find models. Can be a path (as str or Path), or a
#' Context
#' @param names (array(str) (optional)) List of names of the models to retrieve or NULL for all
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
		names <- convert_input(names, "list")
		func_out <- pharmpy$tools$retrieve_models(source, names=names)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' @param model (Model (optional)) Pharmpy model
#' @param results (ModelfitResults (optional)) Results for model
#' @param allometric_variable (str or Expr) Name of the variable to use for allometric scaling (default is WT)
#' @param reference_value (str or numeric or Expr) Reference value for the allometric variable (default is 70)
#' @param parameters (array(str or Expr) (optional)) Parameters to apply scaling to (default is all CL, Q and V parameters)
#' @param initials (array(numeric) (optional)) Initial estimates for the exponents. (default is to use 0.75 for CL and Qs and 1 for Vs)
#' @param lower_bounds (array(numeric) (optional)) Lower bounds for the exponents. (default is 0 for all parameters)
#' @param upper_bounds (array(numeric) (optional)) Upper bounds for the exponents. (default is 2 for all parameters)
#' @param fixed (logical) Should the exponents be fixed or not. (default TRUE
#' @param ... Arguments to pass to tool
#'  
#' @return (AllometryResults) Allometry tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' run_allometry(model=model, results=results, allometric_variable='WGT')
#' }
#' 
#' @export
run_allometry <- function(model=NULL, results=NULL, allometric_variable='WT', reference_value=70, parameters=NULL, initials=NULL, lower_bounds=NULL, upper_bounds=NULL, fixed=TRUE, ...) {
	tryCatch(
	{
		parameters <- convert_input(parameters, "list")
		initials <- convert_input(initials, "list")
		lower_bounds <- convert_input(lower_bounds, "list")
		upper_bounds <- convert_input(upper_bounds, "list")
		func_out <- pharmpy$tools$run_allometry(model=model, results=results, allometric_variable=allometric_variable, reference_value=reference_value, parameters=parameters, initials=initials, lower_bounds=lower_bounds, upper_bounds=upper_bounds, fixed=fixed, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' @param input (Model or str) Read model object/Path to a dataset
#' @param results (ModelfitResults (optional)) Reults of input if input is a model
#' @param modeltype (str) Type of model to build. Valid strings are 'basic_pk', 'pkpd', 'drug_metabolite' and 'tmdd'
#' @param administration (str) Route of administration. Either 'iv', 'oral' or 'ivoral'
#' @param strategy (str) Run algorithm for AMD procedure. Valid options are 'default', 'reevaluation'. Default is 'default'
#' @param cl_init (numeric (optional)) Initial estimate for the population clearance
#' @param vc_init (numeric (optional)) Initial estimate for the central compartment population volume
#' @param mat_init (numeric (optional)) Initial estimate for the mean absorption time (not for iv models)
#' @param b_init (numeric (optional)) Initial estimate for the baseline (PKPD model)
#' @param emax_init (numeric (optional)) Initial estimate for E_max (PKPD model)
#' @param ec50_init (numeric (optional)) Initial estimate for EC_50 (PKPD model)
#' @param met_init (numeric (optional)) Initial estimate for mean equilibration time (PKPD model)
#' @param search_space (str (optional)) MFL for search space for structural model
#' @param lloq_method (str (optional)) Method for how to remove LOQ data. See `transform_blq` for vector of available methods
#' @param lloq_limit (str (optional)) Lower limit of quantification. If NULL LLOQ column from dataset will be used
#' @param allometric_variable (str or Expr (optional)) Variable to use for allometry
#' @param occasion (str (optional)) Name of occasion column
#' @param path (str (optional)) Path to run AMD in
#' @param resume (logical) Whether to allow resuming previous run
#' @param strictness (str (optional)) Strictness criteria
#' @param dv_types (list(str=numeric) (optional)) Dictionary of DV types for TMDD models with multiple DVs.
#' @param mechanistic_covariates (array(str or list(str)) (optional)) List of covariates or tuple of covariate and parameter combination to run in a
#' separate proioritized covsearch run. For instance c("WT", ("CRCL", "CL")).
#' The effects are extracted from the search space for covsearch.
#' @param retries_strategy (str) Whether or not to run retries tool. Valid options are 'skip', 'all_final' or 'final'.
#' Default is 'final'.
#' @param seed (numeric (optional)) Random number generator or seed to be used.
#' @param parameter_uncertainty_method (str (optional)) Parameter uncertainty method.
#' @param ignore_datainfo_fallback (logical) Ignore using datainfo to get information not given by the user. Default is FALSE
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' run_amd(model, results=results)
#' }
#' @seealso
#' run_iiv
#' 
#' run_tool
#' 
#' 
#' @export
run_amd <- function(input, results=NULL, modeltype='basic_pk', administration='oral', strategy='default', cl_init=NULL, vc_init=NULL, mat_init=NULL, b_init=NULL, emax_init=NULL, ec50_init=NULL, met_init=NULL, search_space=NULL, lloq_method=NULL, lloq_limit=NULL, allometric_variable=NULL, occasion=NULL, path=NULL, resume=FALSE, strictness='minimization_successful or (rounding_errors and sigdigs>=0.1)', dv_types=NULL, mechanistic_covariates=NULL, retries_strategy='all_final', seed=NULL, parameter_uncertainty_method=NULL, ignore_datainfo_fallback=FALSE) {
	tryCatch(
	{
		mechanistic_covariates <- convert_input(mechanistic_covariates, "list")
		seed <- convert_input(seed, "int")
		func_out <- pharmpy$tools$run_amd(input, results=results, modeltype=modeltype, administration=administration, strategy=strategy, cl_init=cl_init, vc_init=vc_init, mat_init=mat_init, b_init=b_init, emax_init=emax_init, ec50_init=ec50_init, met_init=met_init, search_space=search_space, lloq_method=lloq_method, lloq_limit=lloq_limit, allometric_variable=allometric_variable, occasion=occasion, path=path, resume=resume, strictness=strictness, dv_types=dv_types, mechanistic_covariates=mechanistic_covariates, retries_strategy=retries_strategy, seed=seed, parameter_uncertainty_method=parameter_uncertainty_method, ignore_datainfo_fallback=ignore_datainfo_fallback)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' run_bootstrap
#' 
#' @description
#' Run bootstrap tool
#' 
#' @param model (Model) Pharmpy model
#' @param results (ModelfitResults (optional)) Results for model
#' @param resamples (numeric) Number of bootstrap resample
#' @param ... Arguments to pass to tool
#'  
#' @return (BootstrapResults) Bootstrap tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' run_bootstrap(model, res, resamples=500)
#' }
#' 
#' @export
run_bootstrap <- function(model, results=NULL, resamples=1, ...) {
	tryCatch(
	{
		resamples <- convert_input(resamples, "int")
		func_out <- pharmpy$tools$run_bootstrap(model, results=results, resamples=resamples, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' @param search_space (str or ModelFeatures) MFL of covariate effects to try
#' @param p_forward (numeric) The p-value to use in the likelihood ratio test for forward steps
#' @param p_backward (numeric) The p-value to use in the likelihood ratio test for backward steps
#' @param max_steps (numeric) The maximum number of search steps to make
#' @param algorithm (str) The search algorithm to use. Currently, 'scm-forward' and
#' 'scm-forward-then-backward' are supported.
#' @param results (ModelfitResults (optional)) Results of model
#' @param model (Model (optional)) Pharmpy model
#' @param max_eval (logical) Limit the number of function evaluations to 3.1 times that of the
#' base model. Default is FALSE.
#' @param adaptive_scope_reduction (logical) Stash all non-significant parameter-covariate effects to be tested
#' after all significant effects have been tested. Once all these have been
#' tested, try adding the stashed effects once more with a regular forward approach.
#' Default is FALSE
#' @param strictness (str (optional)) Strictness criteria
#' @param naming_index_offset (numeric (optional)) index offset for naming of runs. Default is 0
#' @param ... Arguments to pass to tool
#'  
#' @return (COVSearchResults) COVsearch tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' search_space <- 'COVARIATE(c(CL, V), c(AGE, WT), EXP)'
#' res <- run_covsearch(search_space, model=model, results=results)
#' }
#' 
#' @export
run_covsearch <- function(search_space, p_forward=0.01, p_backward=0.001, max_steps=-1, algorithm='scm-forward-then-backward', results=NULL, model=NULL, max_eval=FALSE, adaptive_scope_reduction=FALSE, strictness='minimization_successful or (rounding_errors and sigdigs>=0.1)', naming_index_offset=0, ...) {
	tryCatch(
	{
		max_steps <- convert_input(max_steps, "int")
		naming_index_offset <- convert_input(naming_index_offset, "int")
		func_out <- pharmpy$tools$run_covsearch(search_space, p_forward=p_forward, p_backward=p_backward, max_steps=max_steps, algorithm=algorithm, results=results, model=model, max_eval=max_eval, adaptive_scope_reduction=adaptive_scope_reduction, strictness=strictness, naming_index_offset=naming_index_offset, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' run_estmethod
#' 
#' @description
#' Run estmethod tool.
#' 
#' @param algorithm (str) The algorithm to use (can be 'exhaustive', 'exhaustive_with_update' or 'exhaustive_only_eval')
#' @param methods (array(str) or str (optional)) List of estimation methods to test.
#' Can be specified as 'all', a vector of estimation methods, or NULL (to not test any estimation method)
#' @param solvers (array(str) or str (optional)) List of solver to test. Can be specified as 'all', a vector of solvers, or NULL (to
#' not test any solver)
#' @param parameter_uncertainty_methods (array(str) or str (optional)) List of parameter uncertainty methods to test.
#' Can be specified as 'all', a vector of uncertainty methods, or NULL (to not evaluate any uncertainty)
#' @param compare_ofv (logical) Whether to compare the OFV between candidates. Comparison is made by evaluating using IMP
#' @param results (ModelfitResults (optional)) Results for model
#' @param model (Model (optional)) Pharmpy mode
#' @param ... Arguments to pass to tool
#'  
#' @return (EstMethodResults) Estmethod tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' methods <- c('IMP', 'SAEM')
#' parameter_uncertainty_methods <- NULL
#' run_estmethod(
#'  'reduced', methods=methods, solvers='all',
#'  parameter_uncertainty_methods=parameter_uncertainty_methods, results=results, model=model
#' )
#' }
#' 
#' @export
run_estmethod <- function(algorithm, methods=NULL, solvers=NULL, parameter_uncertainty_methods=NULL, compare_ofv=TRUE, results=NULL, model=NULL, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_estmethod(algorithm, methods=methods, solvers=solvers, parameter_uncertainty_methods=parameter_uncertainty_methods, compare_ofv=compare_ofv, results=results, model=model, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' @param algorithm (str) Which algorithm to run.
#' @param iiv_strategy (str) If/how IIV should be added to start model. Default is 'no_add'.
#' @param rank_type (str) Which ranking type should be used. Default is mBIC.
#' @param cutoff (numeric (optional)) Cutoff for which value of the ranking function that is considered significant. Default
#' is NULL (all models will be ranked)
#' @param results (ModelfitResults (optional)) Results for model
#' @param model (Model (optional)) Pharmpy model
#' @param keep (array(str) (optional)) List of IIVs to keep. Default is "CL"
#' @param strictness (str (optional)) Strictness criteria
#' @param correlation_algorithm (str (optional)) Which algorithm to run for the determining block structure of added IIVs. If NULL, the
#' algorithm is determined based on the 'algorithm' argumen
#' @param ... Arguments to pass to tool
#'  
#' @return (IIVSearchResults) IIVsearch tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' run_iivsearch('td_brute_force', results=results, model=model)
#' }
#' 
#' @export
run_iivsearch <- function(algorithm='top_down_exhaustive', iiv_strategy='no_add', rank_type='mbic', cutoff=NULL, results=NULL, model=NULL, keep=c('CL'), strictness='minimization_successful or (rounding_errors and sigdigs>=0.1)', correlation_algorithm=NULL, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_iivsearch(algorithm=algorithm, iiv_strategy=iiv_strategy, rank_type=rank_type, cutoff=cutoff, results=results, model=model, keep=keep, strictness=strictness, correlation_algorithm=correlation_algorithm, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' @param list_of_parameters (array(str or array(str)) (optional)) List of parameters to test IOV on, if none all parameters with IIV will be tested (default)
#' @param rank_type (str) Which ranking type should be used. Default is mBIC.
#' @param cutoff (numeric (optional)) Cutoff for which value of the ranking type that is considered significant. Default
#' is NULL (all models will be ranked)
#' @param distribution (str) Which distribution added IOVs should have (default is same-as-iiv)
#' @param results (ModelfitResults (optional)) Results for model
#' @param model (Model (optional)) Pharmpy model
#' @param strictness (str (optional)) Strictness criteri
#' @param ... Arguments to pass to tool
#'  
#' @return (IOVSearchResults) IOVSearch tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' run_iovsearch('OCC', results=results, model=model)
#' }
#' 
#' @export
run_iovsearch <- function(column='OCC', list_of_parameters=NULL, rank_type='mbic', cutoff=NULL, distribution='same-as-iiv', results=NULL, model=NULL, strictness='minimization_successful or (rounding_errors and sigdigs>=0.1)', ...) {
	tryCatch(
	{
		list_of_parameters <- convert_input(list_of_parameters, "list")
		func_out <- pharmpy$tools$run_iovsearch(column=column, list_of_parameters=list_of_parameters, rank_type=rank_type, cutoff=cutoff, distribution=distribution, results=results, model=model, strictness=strictness, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' @param model_or_models (Model or array(Model) (optional)) A vector of models are one single model object
#' @param n (numeric (optional)) Number of models to fit. This is only used if the tool is going to be combined with other tools.
#' @param tool (str (optional)) Which tool to use for fitting. Currently, 'nonmem', 'nlmixr', 'rxode' can be used
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
run_modelfit <- function(model_or_models=NULL, n=NULL, tool=NULL, ...) {
	tryCatch(
	{
		n <- convert_input(n, "int")
		func_out <- pharmpy$tools$run_modelfit(model_or_models=model_or_models, n=n, tool=tool, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' @param search_space (str or ModelFeatures) Search space to test. Either as a string or a ModelFeatures object.
#' @param algorithm (str) Algorithm to use.
#' @param iiv_strategy (str) If/how IIV should be added to candidate models. Default is 'absorption_delay'.
#' @param rank_type (str) Which ranking type should be used. Default is mBIC.
#' @param cutoff (numeric (optional)) Cutoff for which value of the ranking function that is considered significant. Default
#' is NULL (all models will be ranked)
#' @param results (ModelfitResults (optional)) Results for model
#' @param model (Model (optional)) Pharmpy model
#' @param strictness (str (optional)) Strictness criteri
#' @param ... Arguments to pass to tool
#'  
#' @return (ModelSearchResults) Modelsearch tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' run_modelsearch('ABSORPTION(ZO);PERIPHERALS(1)', 'exhaustive', results=results, model=model)
#' }
#' 
#' @export
run_modelsearch <- function(search_space, algorithm, iiv_strategy='absorption_delay', rank_type='mbic', cutoff=NULL, results=NULL, model=NULL, strictness='minimization_successful or (rounding_errors and sigdigs >= 0.1)', ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_modelsearch(search_space, algorithm, iiv_strategy=iiv_strategy, rank_type=rank_type, cutoff=cutoff, results=results, model=model, strictness=strictness, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' run_retries
#' 
#' @description
#' Run retries tool.
#' 
#' @param model (Model (optional)) Model object to run retries on. The default is NULL.
#' @param results (ModelfitResults (optional)) Connected ModelfitResults object. The default is NULL.
#' @param number_of_candidates (numeric) Number of retry candidates to run. The default is 5.
#' @param fraction (numeric) Determines allowed increase/decrease from initial parameter estimate. Default is 0.1 (10%)
#' @param use_initial_estimates (logical) Use initial parameter estimates instead of final estimates of input model when creating candidate models.
#' @param strictness (str (optional)) Strictness criteria. The default is "minimization_successful or (rounding_errors and sigdigs >= 0.1)".
#' @param scale (str (optional)) Which scale to update the initial values on. Either normal scale or UCP scale.
#' @param prefix_name (str (optional)) Prefix the candidate model names with given string.
#' @param seed (numeric (optional)) Random number generator or seed to be used
#' @param ... Arguments to pass to tool
#'  
#' @return (RetriesResults) Retries tool results object.
#' 
#' 
#' @export
run_retries <- function(model=NULL, results=NULL, number_of_candidates=5, fraction=0.1, use_initial_estimates=FALSE, strictness='minimization_successful or (rounding_errors and sigdigs >= 0.1)', scale='UCP', prefix_name='', seed=NULL, ...) {
	tryCatch(
	{
		number_of_candidates <- convert_input(number_of_candidates, "int")
		seed <- convert_input(seed, "int")
		func_out <- pharmpy$tools$run_retries(model=model, results=results, number_of_candidates=number_of_candidates, fraction=fraction, use_initial_estimates=use_initial_estimates, strictness=strictness, scale=scale, prefix_name=prefix_name, seed=seed, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' @param model (Model (optional)) Pharmpy model
#' @param results (ModelfitResults (optional)) Results of model
#' @param groups (numeric) The number of bins to use for the time varying models
#' @param p_value (numeric) The p-value to use for the likelihood ratio test
#' @param skip (array(str) (optional)) A vector of models to not attempt.
#' @param max_iter (numeric) Number of iterations to run (1, 2, or 3). For models with BLQ only one iteration is supported.
#' @param dv (numeric (optional)) Which DV to assess the error model for.
#' @param strictness (str (optional)) Strictness criteri
#' @param ... Arguments to pass to tool
#'  
#' @return (RUVSearchResults) Ruvsearch tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' run_ruvsearch(model=model, results=results)
#' }
#' 
#' @export
run_ruvsearch <- function(model=NULL, results=NULL, groups=4, p_value=0.001, skip=NULL, max_iter=3, dv=NULL, strictness='minimization_successful or (rounding_errors and sigdigs>=0.1)', ...) {
	tryCatch(
	{
		groups <- convert_input(groups, "int")
		skip <- convert_input(skip, "list")
		max_iter <- convert_input(max_iter, "int")
		dv <- convert_input(dv, "int")
		func_out <- pharmpy$tools$run_ruvsearch(model=model, results=results, groups=groups, p_value=p_value, skip=skip, max_iter=max_iter, dv=dv, strictness=strictness, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' run_simulation
#' 
#' @description
#' Run the simulation tool.
#' 
#' @param model (Model (optional)) Pharmpy mode
#' @param ... Arguments to pass to tool
#'  
#' @return (SimulationResult) SimulationResults object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model <- set_simulation(model, n=10)
#' run_simulations(model)
#' }
#' 
#' @export
run_simulation <- function(model=NULL, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_simulation(model=model, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' run_structsearch
#' 
#' @description
#' Run the structsearch tool. For more details, see :ref:`structsearch`.
#' 
#' @param type (str) Type of model. Currently only 'drug_metabolite' and 'pkpd'
#' @param search_space (str or ModelFeatures (optional)) Search space to test
#' @param b_init (numeric (optional)) Initial estimate for the baseline for pkpd models.
#' @param emax_init (numeric (optional)) Initial estimate for E_MAX (for pkpd models only).
#' @param ec50_init (numeric (optional)) Initial estimate for EC_50 (for pkpd models only).
#' @param met_init (numeric (optional)) Initial estimate for MET (for pkpd models only).
#' @param results (ModelfitResults (optional)) Results for the start model
#' @param model (Model (optional)) Pharmpy start model
#' @param extra_model (Model (optional)) Optional extra Pharmpy model to use in TMDD structsearch
#' @param strictness (str (optional)) Results for the extra model
#' @param extra_model_results (ModelfitResults (optional)) Strictness criteria
#' @param dv_types (list(str=numeric) (optional)) Dictionary of DV types for TMDD models with multiple DV
#' @param ... Arguments to pass to tool
#'  
#' @return (StructSearchResult) structsearch tool result object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' results <- load_example_modelfit_results("pheno")
#' run_structsearch(model_type='pkpd', results=results, model=model)
#' }
#' 
#' @export
run_structsearch <- function(type, search_space=NULL, b_init=NULL, emax_init=NULL, ec50_init=NULL, met_init=NULL, results=NULL, model=NULL, extra_model=NULL, strictness='minimization_successful or (rounding_errors and sigdigs >= 0.1)', extra_model_results=NULL, dv_types=NULL, ...) {
	tryCatch(
	{
		func_out <- pharmpy$tools$run_structsearch(type, search_space=search_space, b_init=b_init, emax_init=emax_init, ec50_init=ec50_init, met_init=met_init, results=results, model=model, extra_model=extra_model, strictness=strictness, extra_model_results=extra_model_results, dv_types=dv_types, ...)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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
#' Summarize errors and warnings from all runs in a context.
#' 
#' Summarize the errors and warnings found after running the model/models.
#' 
#' @param context (Context) Context in which models were run
#'  
#' @return (data.frame) A DataFrame of errors with model name, category (error or warning), and an integer as index, an empty DataFrame if there were no errors or warnings found.
#' 
#' 
#' @export
summarize_errors <- function(context) {
	tryCatch(
	{
		func_out <- pharmpy$tools$summarize_errors(context)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
		func_out <- reset_index_df(func_out)
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
#' @param models (array(Model)) Input models
#' @param models_res (array(ModelfitResults)) Input results
#'  
#' @return (data.frame | NULL) The summary as a dataframe
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' fit_results <- fit(model)
#' results <- run_tool(
#'  model=model,
#'  mfl='ABSORPTION(ZO);PERIPHERALS(c(1, 2))',
#'  algorithm='reduced_stepwise'
#' summarize_individuals([results$start_model, *results$models])
#' }
#' 
#' @export
summarize_individuals <- function(models, models_res) {
	tryCatch(
	{
		models <- convert_input(models, "list")
		models_res <- convert_input(models_res, "list")
		func_out <- pharmpy$tools$summarize_individuals(models, models_res)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
		func_out <- reset_index_df(func_out)
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
#' |                         | (equation could not be rendered, see API doc on website)
#' |                         | (equation could not be rendered, see API doc on website)
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
#' @param models (array(Model) (optional)) List of models to summarize.
#' @param models_res (array(ModelfitResults) (optional)) Input results
#' @param df (data.frame) Output from a previous call to summarize_individuals.
#'  
#' @return (data.frame) Table with one row per model.
#' 
#' @seealso
#' summarize_individuals : Get raw individual data
#' 
#' 
#' @export
summarize_individuals_count_table <- function(models=NULL, models_res=NULL, df=NULL) {
	tryCatch(
	{
		models <- convert_input(models, "list")
		models_res <- convert_input(models_res, "list")
		func_out <- pharmpy$tools$summarize_individuals_count_table(models=models, models_res=models_res, df=df)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
		func_out <- reset_index_df(func_out)
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
#' and parameter estimates (with errors). If include_all_execution_steps is FALSE,
#' only the last estimation step will be included (note that in that case, the
#' minimization_successful value will be referring to the last estimation step, if
#' last step is evaluation it will go backwards until it finds an estimation step
#' that wasn't an evaluation).
#' 
#' @param context (Context) Context in which models were run
#' @param include_all_execution_steps (logical) Whether to include all estimation steps, default is FALSE
#'  
#' @return (data.frame) A DataFrame of modelfit results with model name and estmation step as index.
#' 
#' 
#' @export
summarize_modelfit_results <- function(context, include_all_execution_steps=FALSE) {
	tryCatch(
	{
		func_out <- pharmpy$tools$summarize_modelfit_results(context, include_all_execution_steps=include_all_execution_steps)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
		func_out <- reset_index_df(func_out)
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
#' @param path (str) Path to results file
#' @param lzma (logical) TRUE for lzma compression. Not applicable to csv file
#' @param csv (logical) Save as csv file 
#' 
#' @export
write_results <- function(results, path, lzma=FALSE, csv=FALSE) {
	tryCatch(
	{
		func_out <- pharmpy$tools$write_results(results, path, lzma=lzma, csv=csv)
		if ('pharmpy.model.results.Results' %in% class(func_out)) {
			func_out <- reset_indices_results(func_out)
		}
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

