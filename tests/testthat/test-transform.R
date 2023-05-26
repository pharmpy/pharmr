context("test-transform")

test_that("pharmr can use pharmpy transformations", {
  
  # requires Python
  skip_on_cran()

  model_start <- load_example_model('pheno')

  model_mat <- add_individual_parameter(model_start, 'MAT')
  expect_match(model_mat$model_code, 'POP_MAT')

  res <- load_example_modelfit_results('pheno')
  pe <- res$parameter_estimates
  model_update <- update_inits(model_start, pe)
  expect_equal(model_start$model_code, model_start$model_code)
  expect_match(model_update$model_code, '0.00469555) ; PTVCL')
  expect_no_match(model_start$model_code, '0.00469555) ; PTVCL')

  individual_parameter_statistics <- calculate_individual_parameter_statistics(model_start, "K=CL/V", pe)

  colnames <- names(individual_parameter_statistics)
  colnames_ref <- c('parameter', 'covariates', 'mean', 'variance', 'stderr')
  expect_equal(colnames, colnames_ref)
})
