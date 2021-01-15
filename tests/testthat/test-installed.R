context("test-installed")

test_that("pharmr is installed", {

    # requires Python
    skip_on_cran()

    library(pharmr)

    model <- read_model_from_string("$PROBLEM base model\n$INPUT ID DV TIME\n$DATA file.csv IGNORE=@\n$PRED\nY=THETA(1)\n$THETA 0.1\n$EST METHOD=1\n")
})
