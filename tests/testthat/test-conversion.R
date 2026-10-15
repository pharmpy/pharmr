context("test-conversion")

test_that("convert_input", {

    # requires Python
    skip_on_cran()

    x <- convert_input(list("1"="A", "2"="B"), "Mapping[int]")
    expect_equal(x, reticulate::py_dict(c(1, 2), c("A", "B")))
})
