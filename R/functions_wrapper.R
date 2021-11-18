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
add_covariate_effect <- function(model, parameter, covariate, effect, operation='*') {
    func_out <- pharmpy$modeling$add_covariate_effect(model, parameter, covariate, effect, operation)
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
#' @param idx (integer) index of estimation step, default is NULL (adds step at the end)
#' @param ... Arguments to pass to EstimationMethod (such as interaction, evaluation)
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
#' 
#' @export
add_estimation_step <- function(model, method, idx=NULL, ...) {
    func_out <- pharmpy$modeling$add_estimation_step(model, method, idx, ...)
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
#' * Logit (*logit*)
#' 
#' For all except exponential the operation input is not needed. Otherwise user specified
#' input is supported. Initial estimates for new etas are 0.09.
#' 
#' @param model (Model) Pharmpy model to add new IIVs to.
#' @param list_of_parameters (str, vector) Name/names of parameter to add new IIVs to.
#' @param expression (str, vector) Effect/effects on eta. Either abbreviated (see above) or custom.
#' @param operation (str, vector, optional) Whether the new IIV should be added or multiplied (default).
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
#' add_iov
#' 
#' remove_iiv
#' 
#' remove_iov
#' 
#' 
#' @export
add_iiv <- function(model, list_of_parameters, expression, operation='*', eta_names=NULL) {
    func_out <- pharmpy$modeling$add_iiv(model, list_of_parameters, expression, operation, eta_names)
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
#' remove_iiv
#' 
#' remove_iov
#' 
#' 
#' @export
add_iov <- function(model, occ, list_of_parameters=NULL, eta_names=NULL) {
    func_out <- pharmpy$modeling$add_iov(model, occ, list_of_parameters, eta_names)
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
#' 2   :math:`{QP1} = {QP1' / 2}`, :math:`{VP1} = {VP1'}`,
#' :math:`{QP2} = {QP1' / 2}` and :math:`{VP2} = {VP1'}`
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
#' append_estimation_step_options
#' 
#' @description
#' Append estimation step options
#' 
#' Appends options to an existing estimation step.
#' 
#' @param model (Model) Pharmpy model
#' @param tool_options (list) any additional tool specific options
#' @param idx (integer) index of estimation step
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
#' 
#' @export
append_estimation_step_options <- function(model, tool_options, idx) {
    func_out <- pharmpy$modeling$append_estimation_step_options(model, tool_options, idx)
    return(py_to_r(func_out))
}

#' @title
#' calculate_eta_shrinkage
#' 
#' @description
#' Calculate eta shrinkage for each eta
#' 
#' @param model (Model) Pharmpy model
#' @param sd (logical) Calculate shrinkage on the standard deviation scale (default is to calculate on the
#'  variance scale)
#'  
#' @return (Series) Shrinkage for each eta
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' calculate_eta_shrinkage(model)
#' calculate_eta_shrinkage(model, sd=TRUE)
#' }
#' @seealso
#' calculate_individual_shrinkage
#' 
#' 
#' @export
calculate_eta_shrinkage <- function(model, sd=FALSE) {
    func_out <- pharmpy$modeling$calculate_eta_shrinkage(model, sd)
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
#' @param exprs (str, sympy expression or iterable of str or sympy expressions) Expressions or equations for parameters of interest. If equations are used
#'  the names of the left hand sides will be used as the names of the parameters.
#' @param rng (Generator or integer) Random number generator or integer seed
#'  
#' @return (data.frame) A DataFrame of statistics indexed on parameter and covariate value.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' rng <- create_rng(23)
#' calculate_individual_parameter_statistics(model, "K=CL/V", rng=rng)
#' }
#' 
#' @export
calculate_individual_parameter_statistics <- function(model, exprs, rng=NULL) {
    df <- pharmpy$modeling$calculate_individual_parameter_statistics(model, exprs, rng)
    df_reset <- df$reset_index()
    return(py_to_r(df_reset))
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
#'  
#' @return (DataFrame) Shrinkage for each eta and individual
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' calculate_individual_shrinkage(model)
#' }
#' @seealso
#' calculate_eta_shrinkage
#' 
#' 
#' @export
calculate_individual_shrinkage <- function(model) {
    func_out <- pharmpy$modeling$calculate_individual_shrinkage(model)
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
#' @param rng (Generator or integer) Random number generator or seed
#'  
#' @return (data.frame) A DataFrame of statistics indexed on parameter and covariate value.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' rng <- create_rng(23)
#' calculate_pk_parameters_statistics(model, rng=rng)
#' }
#' @seealso
#' calculate_individual_parameter_statistics : Calculation of statistics for arbitrary parameters
#' 
#' 
#' @export
calculate_pk_parameters_statistics <- function(model, rng=NULL) {
    df <- pharmpy$modeling$calculate_pk_parameters_statistics(model, rng)
    df_reset <- df$reset_index()
    return(py_to_r(df_reset))
}

#' @title
#' convert_model
#' 
#' @description
#' Convert model to other format
#' 
#' @param model (Model) Model to convert
#' @param to_format (str) Name of format to convert into. Currently supported 'nlmixr' and 'nonmem'
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
    func_out <- pharmpy$modeling$copy_model(model, name)
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
#' the model has results from a previous results. In that case, the correlation will
#' be calculated from individual estimates, otherwise correlation will be set to 10%.
#' 
#' @param model (Model) Pharmpy model
#' @param rvs (vector) Sequence of etas or names of etas to combine. If NULL, all etas that are IIVs and
#'  non-fixed will be used (full block). NULL is default.
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
create_joint_distribution <- function(model, rvs=NULL) {
    func_out <- pharmpy$modeling$create_joint_distribution(model, rvs)
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
#' @return (Results object for tool) 
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
    func_out <- pharmpy$modeling$create_results(path, ...)
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
create_rng <- function(seed) {
    func_out <- pharmpy$modeling$create_rng(seed)
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
#'  
#' @return (data.frame) A series of one evaluated value for each data record
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' evaluate_expression(model, "TVCL*1000")
#' }
#' 
#' @export
evaluate_expression <- function(model, expression) {
    df <- pharmpy$modeling$evaluate_expression(model, expression)
    df_reset <- df$reset_index()
    return(py_to_r(df_reset))
}

#' @title
#' fit
#' 
#' @description
#' Fit models.
#' 
#' @param models (vector) List of models or one single model
#'  
#' @return (Model) Reference to same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' fit(model)
#' }
#' @seealso
#' run_tool
#' 
#' 
#' @export
fit <- function(models) {
    func_out <- pharmpy$modeling$fit(models)
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
#' @param parameter_names (vector or str) one parameter name or a vector of parameter names
#' @param values (vector or numeric) one value or a vector of values (must be equal to number of parameter_names)
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$parameters['THETA(1)']
#' fix_parameters_to(model, 'THETA(1)', 0.5)
#' model$parameters['THETA(1)']
#' }
#' @seealso
#' fix_parameters : Fix parameters
#' 
#' unfix_paramaters : Unfixing parameters
#' 
#' unfix_paramaters_to : Unfixing parameters and setting a new initial estimate in the same
#' 
#' function
#' 
#' 
#' @export
fix_parameters_to <- function(model, parameter_names, values) {
    func_out <- pharmpy$modeling$fix_parameters_to(model, parameter_names, values)
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
#' get_model_covariates
#' 
#' @description
#' List of covariates used in model
#' 
#' A covariate in the model is here defined to be a data item
#' affecting the model prediction excluding dosing items.
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
    func_out <- pharmpy$modeling$get_model_covariates(model, strings)
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
    df <- pharmpy$modeling$get_number_of_observations_per_individual(model)
    df_reset <- df$reset_index()
    return(py_to_r(df_reset))
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
#' get_number_of_observations
#' 
#' get_number_of_observations_per_individual
#' 
#' 
#' @export
get_observations <- function(model) {
    df <- pharmpy$modeling$get_observations(model)
    df_reset <- df$reset_index()
    return(py_to_r(df_reset))
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
#' load_example_model
#' 
#' @description
#' Load an example model
#' 
#' Load an example model from models built into Pharmpy
#' 
#' @param name (str) Name of the model. Currently available model is "pheno"
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
#' plot_individual_predictions
#' 
#' @description
#' Plot DV and predictions grouped on individuals
#' 
#' @param model (Model) Previously run Pharmpy model.
#' @param predictions (vector) A vector of names of predictions to plot. NULL for all available
#' @param individuals (vector) A vector of individuals to include. NULL for all individuals
#'  
#' @return (alt.Chart) Plot
#' 
#' 
#' @export
plot_individual_predictions <- function(model, predictions=NULL, individuals=NULL) {
    func_out <- pharmpy$modeling$plot_individual_predictions(model, predictions, individuals)
    return(py_to_r(func_out))
}

#' @title
#' plot_iofv_vs_iofv
#' 
#' @description
#' Plot individual OFV of two models against each other
#' 
#' @param model (Model) The first model
#' @param other (Model) The second model
#'  
#' @return (alt.Chart) Scatterplot
#' 
#' 
#' @export
plot_iofv_vs_iofv <- function(model, other) {
    func_out <- pharmpy$modeling$plot_iofv_vs_iofv(model, other)
    return(py_to_r(func_out))
}

#' @title
#' predict_influential_individuals
#' 
#' @description
#' Predict influential individuals for a model using a machine learning model.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (pd.Dataframe) Dataframe over the individuals with a `dofv` column containing the raw predicted delta-OFV and an `influential` column with a boolean to tell whether the individual is influential or not.
#' 
#' @seealso
#' predict_influential_outliers
#' 
#' predict_outliers
#' 
#' 
#' @export
predict_influential_individuals <- function(model) {
    df <- pharmpy$modeling$predict_influential_individuals(model)
    df_reset <- df$reset_index()
    return(py_to_r(df_reset))
}

#' @title
#' predict_influential_outliers
#' 
#' @description
#' Predict influential outliers for a model using a machine learning model.
#' 
#' @param model (Model) Pharmpy model
#'  
#' @return (pd.Dataframe) Dataframe over the individuals with a `outliers` and `dofv` columns containing the raw predictions and `influential`, `outlier` and `influential_outlier` boolean columns.
#' 
#' @seealso
#' predict_influential_individuals
#' 
#' predict_outliers
#' 
#' 
#' @export
predict_influential_outliers <- function(model) {
    df <- pharmpy$modeling$predict_influential_outliers(model)
    df_reset <- df$reset_index()
    return(py_to_r(df_reset))
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
#'  
#' @return (pd.Dataframe) Dataframe over the individuals with a `residual` column containing the raw predicted residuals and a `outlier` column with a boolean to tell whether the individual is an outlier or not.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' predict_outliers(model)
#' }
#' @seealso
#' predict_influential_individuals
#' 
#' predict_influential_outliers
#' 
#' 
#' @export
predict_outliers <- function(model) {
    df <- pharmpy$modeling$predict_outliers(model)
    df_reset <- df$reset_index()
    return(py_to_r(df_reset))
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
    func_out <- pharmpy$modeling$read_model_from_database(name, database)
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
#' @return (Model) Read model object
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
#' read_results
#' 
#' @description
#' Read results object from file
#' 
#' @param path (str, Path) Path to results file
#'  
#' @return (Results object for tool) 
#' 
#' @examples
#' \dontrun{
#' res <- read_resuts("results$json")
#' }
#' @seealso
#' create_results
#' 
#' 
#' @export
read_results <- function(path) {
    func_out <- pharmpy$modeling$read_results(path)
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
#' @param idx (integer) index of estimation step to remove
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
#' 
#' @export
remove_iiv <- function(model, to_remove=NULL) {
    func_out <- pharmpy$modeling$remove_iiv(model, to_remove)
    return(py_to_r(func_out))
}

#' @title
#' remove_iov
#' 
#' @description
#' Removes all IOV etas
#' 
#' @param model (Model) Pharmpy model to remove IOV from.
#'  
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
#' 
#' @export
remove_iov <- function(model) {
    func_out <- pharmpy$modeling$remove_iov(model)
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
#' set_lag_time
#' 
#' 
#' @export
remove_lag_time <- function(model) {
    func_out <- pharmpy$modeling$remove_lag_time(model)
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
#' run_tool
#' 
#' @description
#' Run tool workflow
#' 
#' @param name (str) Name of tool to run
#' @param ... Arguments to pass to tool
#' @param ... Arguments to pass to tool
#'  
#' @return (Results object for tool) 
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' res <- run_tool("resmod", model)
#' }
#' 
#' @export
run_tool <- function(name, ...) {
    func_out <- pharmpy$modeling$run_tool(name, ...)
    return(py_to_r(func_out))
}

#' @title
#' sample_individual_estimates
#' 
#' @description
#' Sample individual estimates given their covariance.
#' 
#' @param model (Model) Pharmpy model
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
#' sample_individual_estimates(model, samples_per_id=2, rng=rng)
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
sample_individual_estimates <- function(model, parameters=NULL, samples_per_id=100, rng=NULL) {
    df <- pharmpy$modeling$sample_individual_estimates(model, parameters, samples_per_id, rng)
    df_reset <- df$reset_index()
    return(py_to_r(df_reset))
}

#' @title
#' sample_parameters_from_covariance_matrix
#' 
#' @description
#' Sample parameter vectors using the covariance matrix
#' 
#' If modelfit_results is not provided the results from the model will be used
#' 
#' @param model (Model) Input model
#' @param modelfit_results (ModelfitResults) Alternative results object. Default is to use the one in model
#' @param parameters (vector) Use to only sample a subset of the parameters. NULL means all
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
#' sample_parameters_from_covariance_matrix(model, n=3, rng=rng)
#' }
#' @seealso
#' sample_parameters_uniformly : Sample parameter vectors using uniform distribution
#' 
#' sample_individual_estimates : Sample individual estiates given their covariance
#' 
#' 
#' @export
sample_parameters_from_covariance_matrix <- function(model, modelfit_results=NULL, parameters=NULL, force_posdef_samples=NULL, force_posdef_covmatrix=FALSE, n=1, rng=NULL) {
    df <- pharmpy$modeling$sample_parameters_from_covariance_matrix(model, modelfit_results, parameters, force_posdef_samples, force_posdef_covmatrix, n, rng)
    df_reset <- df$reset_index()
    return(py_to_r(df_reset))
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
#' @param fraction (numeric) Fraction of estimate value to use for distribution bounds
#' @param parameters (data.frame) Names of parameters to use. Default is to use all parameters in the model.
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
#' sample_parameters_uniformly(model, n=3, rng=rng)
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
sample_parameters_uniformly <- function(model, fraction=0.1, parameters=NULL, force_posdef_samples=NULL, n=1, rng=NULL) {
    df <- pharmpy$modeling$sample_parameters_uniformly(model, fraction, parameters, force_posdef_samples, n, rng)
    df_reset <- df$reset_index()
    return(py_to_r(df_reset))
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
    func_out <- pharmpy$modeling$set_additive_error_model(model, data_trans, series_terms)
    return(py_to_r(func_out))
}

#' @title
#' set_bolus_absorption
#' 
#' @description
#' Set or change to bolus absorption rate.
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
    func_out <- pharmpy$modeling$set_combined_error_model(model, data_trans)
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
    func_out <- pharmpy$modeling$set_dtbs_error_model(model, fix_to_log)
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
#' @param ... Arguments to pass to EstimationMethod (such as interaction, evaluation)
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
#' 
#' @export
set_estimation_step <- function(model, method, idx=0, ...) {
    func_out <- pharmpy$modeling$set_estimation_step(model, method, idx, ...)
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
    func_out <- pharmpy$modeling$set_iiv_on_ruv(model, list_of_eps, same_eta, eta_names)
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
#' 
#' @export
set_initial_estimates <- function(model, inits) {
    func_out <- pharmpy$modeling$set_initial_estimates(model, inits)
    return(py_to_r(func_out))
}

#' @title
#' set_lag_time
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
#' set_lag_time(model)
#' }
#' @seealso
#' set_transit_compartments
#' 
#' remove_lag_time
#' 
#' 
#' @export
set_lag_time <- function(model) {
    func_out <- pharmpy$modeling$set_lag_time(model)
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
#' +------------------------+------------------+
#' | Solver                 | NONMEM ADVAN     |
#' +========================+==================+
#' | CVODES                 | ADVAN14          |
#' +------------------------+------------------+
#' | DGEAR                  | ADVAN8           |
#' +------------------------+------------------+
#' | DVERK                  | ADVAN6           |
#' +------------------------+------------------+
#' | IDA                    | ADVAN15          |
#' +------------------------+------------------+
#' | LSODA                  | ADVAN13          |
#' +------------------------+------------------+
#' | LSODI                  | ADVAN9           |
#' +------------------------+------------------+
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
#' @param list_of_eps (str, vector) Name/names of epsilons to apply power effect. If NULL, all epsilons will be used.
#'  NULL is default.
#' @param ipred (Symbol) Symbol to use as IPRED. Default is to autodetect expression for IPRED.
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
set_power_on_ruv <- function(model, list_of_eps=NULL, ipred=NULL) {
    func_out <- pharmpy$modeling$set_power_on_ruv(model, list_of_eps, ipred)
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
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- remove_error_model(load_example_model("pheno"))
#' set_proportional_error_model(model)
#' model$statements$find_assignment("Y")
#' model <- remove_error_model(load_example_model("pheno"))
#' set_proportional_error_model(model, data_trans="log(Y)")
#' model$statements$find_assignment("Y")
#' }
#' @seealso
#' set_additive_error_model : Additive error model
#' 
#' set_combined_error_model : Combined error model
#' 
#' 
#' @export
set_proportional_error_model <- function(model, data_trans=NULL) {
    func_out <- pharmpy$modeling$set_proportional_error_model(model, data_trans)
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
#' set_lag_time
#' 
#' 
#' @export
set_transit_compartments <- function(model, n) {
    func_out <- pharmpy$modeling$set_transit_compartments(model, n)
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
    func_out <- pharmpy$modeling$split_joint_distribution(model, rvs)
    return(py_to_r(func_out))
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
#' @param models (vector, Model) List of models or single model
#' @param include_all_estimation_steps (logical) Whether to include all estimation steps, default is FALSE
#'  
#' @return (data.frame) A DataFrame of modelfit results with model name and estmation step as index.
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' summarize_modelfit_results(c(model))
#' }
#' 
#' @export
summarize_modelfit_results <- function(models, include_all_estimation_steps=FALSE) {
    df <- pharmpy$modeling$summarize_modelfit_results(models, include_all_estimation_steps)
    df_reset <- df$reset_index()
    return(py_to_r(df_reset))
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
    func_out <- pharmpy$modeling$transform_etas_boxcox(model, list_of_etas)
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
    func_out <- pharmpy$modeling$transform_etas_john_draper(model, list_of_etas)
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
    func_out <- pharmpy$modeling$transform_etas_tdist(model, list_of_etas)
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
#' fix_parameters_to : Fixing and setting parameter initial estimates in the same function
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
#' Unix parameters to
#' 
#' Unfix all listed parameters to specified value/values
#' 
#' @param model (Model) Pharmpy model
#' @param parameter_names (vector or str) one parameter name or a vector of parameter names
#' @param values (vector or numeric) one value or a vector of values (must be equal to number of parameter_names)
#'  
#' @return (Model) Reference to the same model object
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' fix_parameters(model, c('THETA(1)', 'THETA(2)', 'THETA(3)'))
#' model$parameters$fix
#' unfix_parameters_to(model, 'THETA(1)', 0.5)
#' model$parameters$fix
#' model$parameters['THETA(1)']
#' }
#' 
#' @export
unfix_parameters_to <- function(model, parameter_names, values) {
    func_out <- pharmpy$modeling$unfix_parameters_to(model, parameter_names, values)
    return(py_to_r(func_out))
}

#' @title
#' update_inits
#' 
#' @description
#' Update initial parameter estimate for a model
#' 
#' Updates initial estimates of population parameters for a model from
#' its modelfit_results. If the model has used initial estimates for
#' individual estimates these will also be updated. If initial estimates
#' 
#' @param model (Model) Pharmpy model to update initial estimates
#' @param force_individual_estimates (logical) Update initial individual estimates even if model din't use them previously.
#'  
#' @return (Model) Reference to the same model
#' 
#' @examples
#' \dontrun{
#' model <- load_example_model("pheno")
#' model$parameters$inits
#' update_inits(model)
#' model$parameters$inits
#' }
#' 
#' @export
update_inits <- function(model, force_individual_estimates=FALSE) {
    func_out <- pharmpy$modeling$update_inits(model, force_individual_estimates)
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
    func_out <- pharmpy$modeling$write_model(model, path, force)
    return(py_to_r(func_out))
}

