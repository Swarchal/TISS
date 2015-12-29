source("ks_test.R")

context("Check signed KS test works")

test_that("signed ks_test",{
    set.seed(12321) # for reproducibility
    a <- rnorm(100, 1, 10)
    b <- rnorm(100, 1, 1)
    out <- ks_test(a, b)
    expect_equal(out, -0.53)
})