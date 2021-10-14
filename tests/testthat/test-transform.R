context("test-transform")

test_that("pharmr can use pharmpy transformations", {
  
  # requires Python
  skip_on_cran()

  model <- load_example_model('pheno')
  add_individual_parameter(model, 'MAT')

  expect_match(toString(model), 'POP_MAT')
})
