context("test-transform")

test_that("pharmr can use pharmpy transformations", {
  
  # requires Python
  skip_on_cran()
  
  model <- read_model('pheno.mod')
  add_individual_parameter(model, 'MAT')
  update_source(model)
  
  expect_match(toString(model), 'POP_MAT')
})
