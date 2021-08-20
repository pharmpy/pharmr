add_covariate_effect <- function(model, parameter, covariate, effect, operation='*') {
    return(pharmpy$modeling$add_covariate_effect(model, parameter, covariate, effect, operation))
}

add_iiv <- function(model, list_of_parameters, expression, operation='*', eta_names=NULL) {
    return(pharmpy$modeling$add_iiv(model, list_of_parameters, expression, operation, eta_names))
}

add_iov <- function(model, occ, list_of_parameters=NULL, eta_names=NULL) {
    return(pharmpy$modeling$add_iov(model, occ, list_of_parameters, eta_names))
}

add_lag_time <- function(model) {
    return(pharmpy$modeling$add_lag_time(model))
}

add_parameter <- function(model, name) {
    return(pharmpy$modeling$add_parameter(model, name))
}

add_peripheral_compartment <- function(model) {
    return(pharmpy$modeling$add_peripheral_compartment(model))
}

additive_error <- function(model) {
    return(pharmpy$modeling$additive_error(model))
}

bolus_absorption <- function(model) {
    return(pharmpy$modeling$bolus_absorption(model))
}

boxcox <- function(model, list_of_etas=NULL) {
    return(pharmpy$modeling$boxcox(model, list_of_etas))
}

combined_error <- function(model) {
    return(pharmpy$modeling$combined_error(model))
}

copy_model <- function(model) {
    return(pharmpy$modeling$copy_model(model))
}

create_results <- function(path) {
    return(pharmpy$modeling$create_results(path))
}

create_rv_block <- function(model, list_of_rvs=NULL) {
    return(pharmpy$modeling$create_rv_block(model, list_of_rvs))
}

explicit_odes <- function(model) {
    return(pharmpy$modeling$explicit_odes(model))
}

first_order_absorption <- function(model) {
    return(pharmpy$modeling$first_order_absorption(model))
}

first_order_elimination <- function(model) {
    return(pharmpy$modeling$first_order_elimination(model))
}

fit <- function(models) {
    return(pharmpy$modeling$fit(models))
}

fix_parameters <- function(model, parameter_names) {
    return(pharmpy$modeling$fix_parameters(model, parameter_names))
}

fix_parameters_to <- function(model, parameter_names, values) {
    return(pharmpy$modeling$fix_parameters_to(model, parameter_names, values))
}

has_additive_error <- function(model) {
    return(pharmpy$modeling$has_additive_error(model))
}

has_combined_error <- function(model) {
    return(pharmpy$modeling$has_combined_error(model))
}

has_proportional_error <- function(model) {
    return(pharmpy$modeling$has_proportional_error(model))
}

iiv_on_ruv <- function(model, list_of_eps=NULL, same_eta=TRUE, eta_names=NULL) {
    return(pharmpy$modeling$iiv_on_ruv(model, list_of_eps, same_eta, eta_names))
}

john_draper <- function(model, list_of_etas=NULL) {
    return(pharmpy$modeling$john_draper(model, list_of_etas))
}

michaelis_menten_elimination <- function(model) {
    return(pharmpy$modeling$michaelis_menten_elimination(model))
}

mixed_mm_fo_elimination <- function(model) {
    return(pharmpy$modeling$mixed_mm_fo_elimination(model))
}

power_on_ruv <- function(model, list_of_eps=NULL) {
    return(pharmpy$modeling$power_on_ruv(model, list_of_eps))
}

proportional_error <- function(model) {
    return(pharmpy$modeling$proportional_error(model))
}

read_model <- function(path) {
    return(pharmpy$modeling$read_model(path))
}

read_model_from_string <- function(code) {
    return(pharmpy$modeling$read_model_from_string(code))
}

read_results <- function(path) {
    return(pharmpy$modeling$read_results(path))
}

remove_error <- function(model) {
    return(pharmpy$modeling$remove_error(model))
}

remove_iiv <- function(model, list_to_remove=NULL) {
    return(pharmpy$modeling$remove_iiv(model, list_to_remove))
}

remove_iov <- function(model) {
    return(pharmpy$modeling$remove_iov(model))
}

remove_lag_time <- function(model) {
    return(pharmpy$modeling$remove_lag_time(model))
}

remove_peripheral_compartment <- function(model) {
    return(pharmpy$modeling$remove_peripheral_compartment(model))
}

seq_zo_fo_absorption <- function(model) {
    return(pharmpy$modeling$seq_zo_fo_absorption(model))
}

set_initial_estimates <- function(model, inits) {
    return(pharmpy$modeling$set_initial_estimates(model, inits))
}

set_name <- function(model, name) {
    return(pharmpy$modeling$set_name(model, name))
}

set_ode_solver <- function(model, solver) {
    return(pharmpy$modeling$set_ode_solver(model, solver))
}

set_peripheral_compartments <- function(model, n) {
    return(pharmpy$modeling$set_peripheral_compartments(model, n))
}

set_transit_compartments <- function(model, n) {
    return(pharmpy$modeling$set_transit_compartments(model, n))
}

split_rv_block <- function(model, list_of_rvs=NULL) {
    return(pharmpy$modeling$split_rv_block(model, list_of_rvs))
}

tdist <- function(model, list_of_etas=NULL) {
    return(pharmpy$modeling$tdist(model, list_of_etas))
}

unfix_parameters <- function(model, parameter_names) {
    return(pharmpy$modeling$unfix_parameters(model, parameter_names))
}

unfix_parameters_to <- function(model, parameter_names, values) {
    return(pharmpy$modeling$unfix_parameters_to(model, parameter_names, values))
}

update_inits <- function(model, force_individual_estimates=FALSE) {
    return(pharmpy$modeling$update_inits(model, force_individual_estimates))
}

update_source <- function(model) {
    return(pharmpy$modeling$update_source(model))
}

write_model <- function(model, path="", force=FALSE) {
    return(pharmpy$modeling$write_model(model, path, force))
}

zero_order_absorption <- function(model) {
    return(pharmpy$modeling$zero_order_absorption(model))
}

zero_order_elimination <- function(model) {
    return(pharmpy$modeling$zero_order_elimination(model))
}

