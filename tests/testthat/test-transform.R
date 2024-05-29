context("test-transform")

test_that("pharmr can use pharmpy transformations", {
  
  # requires Python
  skip_on_cran()

  model_start <- load_example_model('pheno')

  model_mat <- add_individual_parameter(model_start, 'MAT')
  expect_match(model_mat$code, 'POP_MAT')

  res <- load_example_modelfit_results('pheno')
  pe <- res$parameter_estimates
  model_update <- set_initial_estimates(model_start, pe)
  expect_equal(model_start$code, model_start$code)
  expect_match(model_update$code, '0.00469555) ; POP_CL')
  expect_no_match(model_start$code, '0.00469555) ; POP_CL')

  individual_parameter_statistics <- calculate_individual_parameter_statistics(model_start, "K=CL/V", pe)

  colnames <- names(individual_parameter_statistics)
  colnames_ref <- c('parameter', 'covariates', 'mean', 'variance', 'stderr')
  expect_equal(colnames, colnames_ref)
})
