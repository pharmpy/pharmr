#' @title
#' add_covariate_effect
#' 
#' @description
#' Adds covariate effect to :class:`pharmpy.model`. The following effects have templates:
#' 
#' * Linear function for continuous covariates (*lin*)
#' * Function:
#' 
#' .. math::
#' 
#' \text{coveff} = 1 + \text{theta} * (\text{cov} - \text{median})
#' 
#' * Init:  0.001
#' * Upper:
#' * If median of covariate equals minimum: :math:`100,000`
#' * Otherwise: :math:`\frac{1}{\text{median} - \text{min}}`
#' * Lower:
#' * If median of covariate equals maximum: :math:`-100,000`
#' * Otherwise: :math:`\frac{1}{\text{median} - \text{max}}`
#' * Linear function for categorical covariates (*cat*)
#' * Function:
#' 
#' * If covariate is most common category:
#' 
#' .. math::
#' 
#' \text{coveff} = 1
#' 
#' * For each additional category:
#' 
#' .. math::
#' 
#' \text{coveff} = 1 + \text{theta}
#' 
#' * Init: :math:`0.001`
#' * Upper: :math:`100,000`
#' * Lower: :math:`-100,000`
#' * Piecewise linear function/"hockey-stick", continuous covariates only (*piece_lin*)
#' * Function:
#' * If cov <= median:
#' 
#' .. math::
#' 
#' \text{coveff} = 1 + \text{theta1} * (\text{cov} - \text{median})
#' 
#' * If cov > median:
#' 
#' .. math::
#' 
#' \text{coveff} = 1 + \text{theta2} * (\text{cov} - \text{median})
#' 
#' 
#' * Init: :math:`0.001`
#' * Upper:
#' * For first state: :math:`\frac{1}{\text{median} - \text{min}}`
#' * Otherwise: :math:`100,000`
#' * Lower:
#' * For first state: :math:`-100,000`
#' * Otherwise: :math:`\frac{1}{\text{median} - \text{max}}`
#' * Exponential function, continuous covariates only (*exp*)
#' * Function:
#' 
#' .. math::
#' 
#' \text{coveff} = \exp(\text{theta} * (\text{cov} - \text{median}))
#' 
#' * Init:
#' * If lower > 0.001 or upper < 0.001: :math:`\frac{\text{upper} - \text{lower}}{2}`
#' * If estimated init is 0: :math:`\frac{\text{upper}}{2}`
#' * Otherwise: :math:`0.001`
#' * Upper:
#' * If min - median = 0 or max - median = 0: :math:`100`
#' * Otherwise:
#' 
#' .. math::
#' 
#' \min(\frac{\log(0.01)}{\text{min} - \text{median}},
#' \frac{\log(100)}{\text{max} - \text{median}})
#' * Lower:
#' * If min - median = 0 or max - median = 0: :math:`0.01`
#' * Otherwise:
#' 
#' .. math::
#' 
#' \max(\frac{\log(0.01)}{\text{max} - \text{median}},
#' \frac{\log(100)}{\text{min} - \text{median}})
#' 
#' * Power function, continuous covariates only (*pow*)
#' * Function:
#' 
#' .. math::
#' 
#' \text{coveff} = (\frac{\text{cov}}{\text{median}})^\text{theta}
#' 
#' * Init: :math:`0.001`
#' * Upper: :math:`100,000`
#' * Lower: :math:`-100`
#' 
#' 
#' 
#' @param model (Model) Pharmpy model to add covariate effect to.
#' @param parameter (str) Name of parameter to add covariate effect to.
#' @param covariate (str) Name of covariate.
#' @param effect (str) Type of covariate effect. May be abbreviated covariate effect (see above) or custom.
#' @param operation (str, optional) Whether the covariate effect should be added or multiplied (default).
#' 
#' @export
add_covariate_effect <- function(model, parameter, covariate, effect, operation='*') {
    return(pharmpy$modeling$add_covariate_effect(model, parameter, covariate, effect, operation))
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
#' 
#' @param model (Model) Pharmpy model
#' @param method (str) estimation method to change to
#' @param interaction (logical) whether to use interaction or not, default is true
#' @param options (list) any additional options. Note that this removes old options
#' @param est_idx (int) index of estimation step, default is None (adds step last)
#'  
#' 
#' @return model (Model)
#' @export
add_estimation_step <- function(model, method, interaction=TRUE, options=list(), est_idx=NULL) {
    return(pharmpy$modeling$add_estimation_step(model, method, interaction, options, est_idx))
}

#' @title
#' add_iiv
#' 
#' @description
#' Adds IIVs to :class:`pharmpy.model`. Effects that currently have templates are:
#' 
#' * Additive (*add*)
#' * Proportional (*prop*)
#' * Exponential (*exp*)
#' * Logit (*logit*)
#' 
#' For all except exponential the operation input is not needed. Otherwise user specified
#' input is supported. Initial estimates for new etas are 0.09.
#' 
#' 
#' @param model (Model) Pharmpy model to add new IIVs to.
#' @param list_of_parameters (str, vector) Name/names of parameter to add new IIVs to.
#' @param expression (str, vector) Effect/effects on eta. Either abbreviated (see above) or custom.
#' @param operation (str, vector, optional) Whether the new IIV should be added or multiplied (default).
#'  eta_names: str, list, optional
#'  Custom name/names of new eta
#' 
#' @export
add_iiv <- function(model, list_of_parameters, expression, operation='*', eta_names=NULL) {
    return(pharmpy$modeling$add_iiv(model, list_of_parameters, expression, operation, eta_names))
}

#' @title
#' add_iov
#' 
#' @description
#' Adds IOVs to :class:`pharmpy.model`. Initial estimate of new IOVs are 10% of the IIV eta
#' it is based on.
#' 
#' 
#' @param model (Model) Pharmpy model to add new IOVs to.
#' @param occ (str) Name of occasion column.
#' @param list_of_parameters (str, vector) List of names of parameters and random variables. Accepts random variable names, parameter
#'  names, or a mix of both.
#'  eta_names: str, list
#'  Custom names of new etas. Must be equal to the number of input etas times the number of
#'  categories for occasion.
#' 
#' @export
add_iov <- function(model, occ, list_of_parameters=NULL, eta_names=NULL) {
    return(pharmpy$modeling$add_iov(model, occ, list_of_parameters, eta_names))
}

#' @title
#' add_lag_time
#' 
#' @description
#' Add lag time to the dose compartment of model. Initial estimate for lag time is set the
#' previous lag time if available, otherwise it is set to the time of first observation/2 is
#' used.
#' 
#' @export
add_lag_time <- function(model) {
    return(pharmpy$modeling$add_lag_time(model))
}

#' @title
#' add_parameter
#' 
#' @description
#' Add an individual or pk parameter to a model
#' 
#' @export
add_parameter <- function(model, name) {
    return(pharmpy$modeling$add_parameter(model, name))
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
#' 1   :math:`\mathsf{CL} = \mathsf{CL'}`, :math:`\mathsf{VC} = \mathsf{VC'}`,
#' :math:`\mathsf{QP1} = \mathsf{CL'}` and :math:`\mathsf{VP1} = \mathsf{VC'} \cdot 0.05`
#' 2   :math:`\mathsf{QP1} = \mathsf{QP1' / 2}`, :math:`\mathsf{VP1} = \mathsf{VP1'}`,
#' :math:`\mathsf{QP2} = \mathsf{QP1' / 2}` and :math:`\mathsf{VP2} = \mathsf{VP1'}`
#' ==  ===================================================
#' 
#' @export
add_peripheral_compartment <- function(model) {
    return(pharmpy$modeling$add_peripheral_compartment(model))
}

#' @title
#' additive_error
#' 
#' @description
#' Set an additive error model. Initial estimate for new sigma is :math:`(min(DV)/2)²`.
#' 
#' The error function being applied depends on the data transformation.
#' 
#' +------------------------+----------------------------------------+
#' | Data transformation    | Additive error                         |
#' +========================+========================================+
#' | :math:`y`              | :math:`f + \epsilon_1`                 |
#' +------------------------+----------------------------------------+
#' | :math:`log(y)`         | :math:`\log(f) + \frac{\epsilon_1}{f}` |
#' +------------------------+----------------------------------------+
#' 
#' 
#' @param model (Model) Set error model for this model
#' @param data_trans (str or expression) A data transformation expression or None (default) to use the transformation
#'  specified by the model.
#' 
#' @export
additive_error <- function(model, data_trans=NULL) {
    return(pharmpy$modeling$additive_error(model, data_trans))
}

#' @title
#' bolus_absorption
#' 
#' @description
#' Set or change to bolus absorption rate.
#' 
#' 
#' @param model (Model) Model to set or change absorption rate
#' 
#' @export
bolus_absorption <- function(model) {
    return(pharmpy$modeling$bolus_absorption(model))
}

#' @title
#' boxcox
#' 
#' @description
#' Applies a boxcox transformation to specified etas from a :class:`pharmpy.model`. Initial
#' estimate for lambda is 0.1 with bounds (-3, 3).
#' 
#' 
#' @param model (Model) Pharmpy model to apply boxcox transformation to.
#' @param list_of_etas (str, vector) Name/names of etas to transform. If None, all etas will be transformed (default).
#' 
#' @export
boxcox <- function(model, list_of_etas=NULL) {
    return(pharmpy$modeling$boxcox(model, list_of_etas))
}

#' @title
#' combined_error
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
#' | :math:`y`              | :math:`f + f \epsilon_1 + \epsilon_2`               |
#' +------------------------+-----------------------------------------------------+
#' | :math:`log(y)`         | :math:`\log(f) + \epsilon_1 + \frac{\epsilon_2}{f}` |
#' +------------------------+-----------------------------------------------------+
#' 
#' 
#' @param model (Model) Set error model for this model
#' @param data_trans (str or expression) A data transformation expression or None (default) to use the transformation
#'  specified by the model.
#' 
#' @export
combined_error <- function(model, data_trans=NULL) {
    return(pharmpy$modeling$combined_error(model, data_trans))
}

#' @title
#' convert_model
#' 
#' @description
#' Convert model to other format
#' 
#' 
#' @param model (Model) Model to convert
#' @param to (str) Name of format to convert into. Currently supported 'nlmixr'
#'  
#'  Results
#'  Model
#'  New model object with new underlying model format.
#' 
#' @export
convert_model <- function(model, to) {
    return(pharmpy$modeling$convert_model(model, to))
}

#' @title
#' copy_model
#' 
#' @description
#' Copies model to a new model object
#' 
#' @export
copy_model <- function(model) {
    return(pharmpy$modeling$copy_model(model))
}

#' @title
#' create_results
#' 
#' @export
create_results <- function(path) {
    return(pharmpy$modeling$create_results(path))
}

#' @title
#' create_rv_block
#' 
#' @description
#' Creates a full or partial block structure of etas. The etas must be IIVs and cannot
#' be fixed. Initial estimates for covariance between the etas is dependent on whether
#' the model has results from a previous results. In that case, the correlation will
#' be calculated from individual estimates, otherwise correlation will be set to 10%.
#' 
#' 
#' @param model (Model) Pharmpy model to create block effect on.
#' @param list_of_rvs (vector) List of etas to create a block structure from. If None, all etas that are IIVs and
#'  non-fixed will be used (full block). None is default.
#' 
#' @export
create_rv_block <- function(model, list_of_rvs=NULL) {
    return(pharmpy$modeling$create_rv_block(model, list_of_rvs))
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
#' 
#' @param expression (str or sympy expression) Expression to evaluate
#'  
#' 
#'  pd.Series
#'  A series of one evaluated value for each data record
#' 
#' @export
evaluate_expression <- function(model, expression) {
    return(pharmpy$modeling$evaluate_expression(model, expression))
}

#' @title
#' explicit_odes
#' 
#' @description
#' Convert model from compartmental system to explicit ODE system
#' or do nothing if it already has an explicit ODE system
#' 
#' @export
explicit_odes <- function(model) {
    return(pharmpy$modeling$explicit_odes(model))
}

#' @title
#' first_order_absorption
#' 
#' @description
#' Set or change to first order absorption rate. Initial estimate for absorption rate is set
#' the previous rate if available, otherwise it is set to the time of first observation/2 is used.
#' 
#' 
#' @param model (Model) Model to set or change to use first order absorption rate
#' 
#' @export
first_order_absorption <- function(model) {
    return(pharmpy$modeling$first_order_absorption(model))
}

#' @title
#' first_order_elimination
#' 
#' @export
first_order_elimination <- function(model) {
    return(pharmpy$modeling$first_order_elimination(model))
}

#' @title
#' fit
#' 
#' @export
fit <- function(models) {
    return(pharmpy$modeling$fit(models))
}

#' @title
#' fix_parameters
#' 
#' @description
#' Fix parameters
#' 
#' Fix all listed parameters
#' 
#' 
#' @param model (Model) Pharmpy model
#' @param parameter_names (vector or str) one parameter name or a list of parameter names
#'  
#' 
#' @return model (Model)
#' @export
fix_parameters <- function(model, parameter_names) {
    return(pharmpy$modeling$fix_parameters(model, parameter_names))
}

#' @title
#' fix_parameters_to
#' 
#' @description
#' Fix parameters to
#' 
#' Fix all listed parameters to specified value/values
#' 
#' 
#' @param model (Model) Pharmpy model
#' @param parameter_names (vector or str) one parameter name or a list of parameter names
#' @param values (vector or int) one value or a list of values (must be equal to number of parameter_names)
#'  
#' 
#' @return model (Model)
#' @export
fix_parameters_to <- function(model, parameter_names, values) {
    return(pharmpy$modeling$fix_parameters_to(model, parameter_names, values))
}

#' @title
#' has_additive_error
#' 
#' @description
#' Check if a model has an additive error model
#' 
#' 
#' @param model (Model) The model to check
#' 
#' @export
has_additive_error <- function(model) {
    return(pharmpy$modeling$has_additive_error(model))
}

#' @title
#' has_combined_error
#' 
#' @description
#' Check if a model has a combined additive and proportinal error model
#' 
#' 
#' @param model (Model) The model to check
#' 
#' @export
has_combined_error <- function(model) {
    return(pharmpy$modeling$has_combined_error(model))
}

#' @title
#' has_proportional_error
#' 
#' @description
#' Check if a model has a proportional error model
#' 
#' 
#' @param model (Model) The model to check
#' 
#' @export
has_proportional_error <- function(model) {
    return(pharmpy$modeling$has_proportional_error(model))
}

#' @title
#' iiv_on_ruv
#' 
#' @description
#' Multiplies epsilons with exponential (new) etas. Initial estimates for new etas are 0.09.
#' 
#' 
#' @param model (Model) Pharmpy model to apply IIV on epsilons.
#' @param list_of_eps (str, vector) Name/names of epsilons to multiply with exponential etas. If None, all epsilons will
#'  be chosen. None is default.
#' @param same_eta (logical) Boolean of whether all RUVs from input should use the same new ETA or if one ETA
#'  should be created for each RUV. True is default.
#'  eta_names: str, list
#'  Custom names of new etas. Must be equal to the number epsilons or 1 if same eta.
#' 
#' @export
iiv_on_ruv <- function(model, list_of_eps=NULL, same_eta=TRUE, eta_names=NULL) {
    return(pharmpy$modeling$iiv_on_ruv(model, list_of_eps, same_eta, eta_names))
}

#' @title
#' john_draper
#' 
#' @description
#' Applies a John Draper transformation [1]_ to specified etas from a
#' :class:`pharmpy.model`. Initial estimate for lambda is 0.1 with bounds (-3, 3).
#' 
#' .. [1] John, J., Draper, N. (1980). An Alternative Family of Transformations.
#' Journal of the Royal Statistical Society. Series C (Applied Statistics),
#' 29(2), 190-197. doi:10.2307/2986305
#' 
#' 
#' @param model (Model) Pharmpy model to apply John Draper transformation to.
#' @param list_of_etas (str, vector) Name/names of etas to transform. If None, all etas will be transformed (default).
#' 
#' @export
john_draper <- function(model, list_of_etas=NULL) {
    return(pharmpy$modeling$john_draper(model, list_of_etas))
}

#' @title
#' michaelis_menten_elimination
#' 
#' @description
#' Sets elimination to Michaelis-Menten. Initial estimate for CLMM is set to CL and KM is set to
#' :math:`2*max(DV)`.
#' 
#' @export
michaelis_menten_elimination <- function(model) {
    return(pharmpy$modeling$michaelis_menten_elimination(model))
}

#' @title
#' mixed_mm_fo_elimination
#' 
#' @description
#' Sets elimination to mixed Michaelis-Menten and first order. Initial estimate for CLMM is set
#' to CL/2 and KM is set to :math:`2*max(DV)`.
#' 
#' @export
mixed_mm_fo_elimination <- function(model) {
    return(pharmpy$modeling$mixed_mm_fo_elimination(model))
}

#' @title
#' ninds
#' 
#' @description
#' Retrieve the number of individuals in the model dataset
#' 
#' @export
ninds <- function(model) {
    return(pharmpy$modeling$ninds(model))
}

#' @title
#' nobs
#' 
#' @description
#' Retrieve the total number of observations in the model dataset
#' 
#' @export
nobs <- function(model) {
    return(pharmpy$modeling$nobs(model))
}

#' @title
#' nobsi
#' 
#' @description
#' Number of observations for each individual
#' 
#' @export
nobsi <- function(model) {
    return(pharmpy$modeling$nobsi(model))
}

#' @title
#' power_on_ruv
#' 
#' @description
#' Applies a power effect to provided epsilons. Initial estimates for new thetas are 1 if the error
#' model is proportional, otherwise they are 0.1.
#' 
#' 
#' @param model (Model) Pharmpy model to create block effect on.
#' @param list_of_eps (str, vector) Name/names of epsilons to apply power effect. If None, all epsilons will be used.
#'  None is default.
#' 
#' @export
power_on_ruv <- function(model, list_of_eps=NULL) {
    return(pharmpy$modeling$power_on_ruv(model, list_of_eps))
}

#' @title
#' proportional_error
#' 
#' @description
#' Set a proportional error model. Initial estimate for new sigma is 0.09.
#' 
#' The error function being applied depends on the data transformation.
#' 
#' +------------------------+----------------------------------------+
#' | Data transformation    | Proportional error                     |
#' +========================+========================================+
#' | :math:`y`              | :math:`f + f \epsilon_1`               |
#' +------------------------+----------------------------------------+
#' | :math:`log(y)`         | :math:`\log(f) + \epsilon_1`           |
#' +------------------------+----------------------------------------+
#' 
#' 
#' @param model (Model) Set error model for this model
#' @param data_trans (str or expression) A data transformation expression or None (default) to use the transformation
#'  specified by the model.
#' 
#' @export
proportional_error <- function(model, data_trans=NULL) {
    return(pharmpy$modeling$proportional_error(model, data_trans))
}

#' @title
#' read_model
#' 
#' @description
#' Read model from file
#' 
#' @export
read_model <- function(path) {
    return(pharmpy$modeling$read_model(path))
}

#' @title
#' read_model_from_string
#' 
#' @description
#' Read model directly from the model code in a string
#' 
#' @export
read_model_from_string <- function(code) {
    return(pharmpy$modeling$read_model_from_string(code))
}

#' @title
#' read_results
#' 
#' @export
read_results <- function(path) {
    return(pharmpy$modeling$read_results(path))
}

#' @title
#' remove_error
#' 
#' @description
#' Remove error model.
#' 
#' 
#' @param model (Model) Remove error model for this model
#' 
#' @export
remove_error <- function(model) {
    return(pharmpy$modeling$remove_error(model))
}

#' @title
#' remove_estimation_step
#' 
#' @description
#' Remove estimation step
#' 
#' 
#' @param model (Model) Pharmpy model
#' @param est_idx (int) index of estimation step to remove
#'  
#' 
#' @return model (Model)
#' @export
remove_estimation_step <- function(model, est_idx) {
    return(pharmpy$modeling$remove_estimation_step(model, est_idx))
}

#' @title
#' remove_iiv
#' 
#' @description
#' Removes all IIV omegas given a list with eta names and/or parameter names.
#' 
#' 
#' @param model (Model) Pharmpy model to create block effect on.
#' @param list_to_remove (str, vector) Name/names of etas and/or name/names of individual parameters to remove.
#'  If None, all etas that are IIVs will be removed. None is default.
#' 
#' @export
remove_iiv <- function(model, list_to_remove=NULL) {
    return(pharmpy$modeling$remove_iiv(model, list_to_remove))
}

#' @title
#' remove_iov
#' 
#' @description
#' Removes all IOV omegas.
#' 
#' 
#' @param model (Model) Pharmpy model to remove IOV from.
#' 
#' @export
remove_iov <- function(model) {
    return(pharmpy$modeling$remove_iov(model))
}

#' @title
#' remove_lag_time
#' 
#' @description
#' Remove lag time from the dose compartment of model.
#' 
#' @export
remove_lag_time <- function(model) {
    return(pharmpy$modeling$remove_lag_time(model))
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
#' 2   :math:`\mathsf{CL} = \mathsf{CL'}`,
#' :math:`\mathsf{QP1} = \mathsf{CL'}` and :math:`\mathsf{VP1} = \mathsf{VC'} \cdot 0.05`
#' 3   :math:`\mathsf{QP1} = (\mathsf{QP1'} + \mathsf{QP2'}) / 2`,
#' :math:`\mathsf{VP1} = \mathsf{VP1'} + \mathsf{VP2'}`
#' ==  ===================================================
#' 
#' @export
remove_peripheral_compartment <- function(model) {
    return(pharmpy$modeling$remove_peripheral_compartment(model))
}

#' @title
#' seq_zo_fo_absorption
#' 
#' @description
#' Set or change to sequential zero order first order absorption rate. Initial estimate for
#' absorption rate is set the previous rate if available, otherwise it is set to the time of
#' first observation/2 is used.
#' 
#' 
#' @param model (Model) Model to set or change absorption rate
#' 
#' @export
seq_zo_fo_absorption <- function(model) {
    return(pharmpy$modeling$seq_zo_fo_absorption(model))
}

#' @title
#' set_dtbs_error
#' 
#' @description
#' Dynamic transform both sides
#' 
#' @export
set_dtbs_error <- function(model) {
    return(pharmpy$modeling$set_dtbs_error(model))
}

#' @title
#' set_estimation_step
#' 
#' @description
#' Set estimation step
#' 
#' Sets estimation step for a model. Methods currently supported are:
#' FO, FOCE, ITS, LAPLACE, IMPMAP, IMP, SAEM
#' 
#' 
#' @param model (Model) Pharmpy model
#' @param method (str) estimation method to change to
#' @param interaction (logical) whether to use interaction or not, default is true
#' @param options (list) any additional options. Note that this removes old options
#' @param est_idx (int) index of estimation step, default is 0 (first estimation step)
#'  
#' 
#' @return model (Model)
#' @export
set_estimation_step <- function(model, method, interaction=TRUE, options=list(), est_idx=0) {
    return(pharmpy$modeling$set_estimation_step(model, method, interaction, options, est_idx))
}

#' @title
#' set_initial_estimates
#' 
#' @description
#' Set initial estimates
#' 
#' 
#' @param model (Model) Pharmpy model
#' @param inits (list) A dictionary of parameter init for parameters to change
#'  
#' 
#' @return model (Model)
#' @export
set_initial_estimates <- function(model, inits) {
    return(pharmpy$modeling$set_initial_estimates(model, inits))
}

#' @title
#' set_name
#' 
#' @description
#' Sets name of model object
#' 
#' @export
set_name <- function(model, name) {
    return(pharmpy$modeling$set_name(model, name))
}

#' @title
#' set_ode_solver
#' 
#' @export
set_ode_solver <- function(model, solver) {
    return(pharmpy$modeling$set_ode_solver(model, solver))
}

#' @title
#' set_peripheral_compartments
#' 
#' @export
set_peripheral_compartments <- function(model, n) {
    return(pharmpy$modeling$set_peripheral_compartments(model, n))
}

#' @title
#' set_transit_compartments
#' 
#' @description
#' Set the number of transit compartments of model. Initial estimate for absorption rate is
#' set the previous rate if available, otherwise it is set to the time of first observation/2
#' is used.
#' 
#' @export
set_transit_compartments <- function(model, n) {
    return(pharmpy$modeling$set_transit_compartments(model, n))
}

#' @title
#' set_weighted_error_model
#' 
#' @description
#' Encode error model with one epsilon and W as weight
#' 
#' @export
set_weighted_error_model <- function(model) {
    return(pharmpy$modeling$set_weighted_error_model(model))
}

#' @title
#' split_rv_block
#' 
#' @description
#' Splits a block structure given a list of etas to separate.
#' 
#' 
#' @param model (Model) Pharmpy model to create block effect on.
#' @param list_of_rvs (str, vector) Name/names of etas to split from block structure. If None, all etas that are IIVs and
#'  non-fixed will become single. None is default.
#' 
#' @export
split_rv_block <- function(model, list_of_rvs=NULL) {
    return(pharmpy$modeling$split_rv_block(model, list_of_rvs))
}

#' @title
#' tdist
#' 
#' @description
#' Applies a t-distribution transformation to specified etas from a :class:`pharmpy.model`. Initial
#' estimate for degrees of freedom is 80 with bounds (3, 100).
#' 
#' 
#' @param model (Model) Pharmpy model to apply t distribution transformation to.
#' @param list_of_etas (str, vector) Name/names of etas to transform. If None, all etas will be transformed (default).
#' 
#' @export
tdist <- function(model, list_of_etas=NULL) {
    return(pharmpy$modeling$tdist(model, list_of_etas))
}

#' @title
#' theta_as_stdev
#' 
#' @description
#' Use thetas to estimate standard deviation of error
#' 
#' @export
theta_as_stdev <- function(model) {
    return(pharmpy$modeling$theta_as_stdev(model))
}

#' @title
#' unfix_parameters
#' 
#' @description
#' Unfix parameters
#' 
#' Unfix all listed parameters
#' 
#' 
#' @param model (Model) Pharmpy model
#' @param parameter_names (vector or str) one parameter name or a list of parameter names
#'  
#' 
#' @return model (Model)
#' @export
unfix_parameters <- function(model, parameter_names) {
    return(pharmpy$modeling$unfix_parameters(model, parameter_names))
}

#' @title
#' unfix_parameters_to
#' 
#' @description
#' Unix parameters to
#' 
#' Unfix all listed parameters to specified value/values
#' 
#' 
#' @param model (Model) Pharmpy model
#' @param parameter_names (vector or str) one parameter name or a list of parameter names
#' @param values (vector or int) one value or a list of values (must be equal to number of parameter_names)
#'  
#' 
#' @return model (Model)
#' @export
unfix_parameters_to <- function(model, parameter_names, values) {
    return(pharmpy$modeling$unfix_parameters_to(model, parameter_names, values))
}

#' @title
#' update_inits
#' 
#' @description
#' Updates initial estimates from previous output. Can be forced if no initial
#' individual estimates have been read.
#' 
#' 
#' @param model (Model) Pharmpy model to create block effect on.
#' @param force_individual_estimates (logical) Whether update of initial individual estimates should be forced.
#' 
#' @export
update_inits <- function(model, force_individual_estimates=FALSE) {
    return(pharmpy$modeling$update_inits(model, force_individual_estimates))
}

#' @title
#' update_source
#' 
#' @description
#' Update source
#' 
#' Let the code of the underlying source language be updated to reflect
#' changes in the model object.
#' 
#' @export
update_source <- function(model) {
    return(pharmpy$modeling$update_source(model))
}

#' @title
#' write_model
#' 
#' @description
#' Write model to file
#' 
#' @export
write_model <- function(model, path='', force=FALSE) {
    return(pharmpy$modeling$write_model(model, path, force))
}

#' @title
#' zero_order_absorption
#' 
#' @description
#' Set or change to zero order absorption rate. Initial estimate for absorption rate is set
#' the previous rate if available, otherwise it is set to the time of first observation/2 is
#' used.
#' 
#' 
#' @param model (Model) Model to set or change to first order absorption rate
#' 
#' @export
zero_order_absorption <- function(model) {
    return(pharmpy$modeling$zero_order_absorption(model))
}

#' @title
#' zero_order_elimination
#' 
#' @description
#' Sets elimination to zero order. Initial estimate for KM is set to 1% of smallest
#' observation.
#' 
#' @export
zero_order_elimination <- function(model) {
    return(pharmpy$modeling$zero_order_elimination(model))
}

