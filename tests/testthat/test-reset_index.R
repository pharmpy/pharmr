context("test-transform")

test_that("multi index can be reset", {
  
  # requires Python
  skip_on_cran()
  
  res <- read_results('results.json')
  df <- res$covariate_effects

  expect_equal(names(df), c("parameter", "covariate", "condition", "p5", "mean", "p95"))
})
