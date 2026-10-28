# SPDX-FileCopyrightText: 2021-2026 Rikard Nordgren and Stella Belin
#
# SPDX-License-Identifier: LGPL-3.0-or-later

context("test-transform")

test_that("multi index can be reset", {
  
  # requires Python
  skip_on_cran()
  
  res <- read_results('results.json')
  df <- res$covariate_effects

  expect_equal(names(df), c("parameter", "covariate", "condition", "p5", "mean", "p95"))
})
