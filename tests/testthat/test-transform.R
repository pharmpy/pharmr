context("test-transform")

test_that("pharmr can use pharmpy transformations", {
  
  # requires Python
  skip_on_cran()

  model <- load_example_model('pheno')
  model <- add_individual_parameter(model, 'MAT')

  expect_match(model$model_code, 'POP_MAT')
})
