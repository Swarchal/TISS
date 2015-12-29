context("Check signed KS test works")

set.seed(12321) # for reproducibility
a <- rnorm(100, 1, 10)
b <- rnorm(100, 1, 1)
out <- ks_test(a, b)

test_that("signed ks_test returns a number",{
    expect_is(out, "numeric")
})

test_that("signed ks_test returns same number as base ks.test",{
    base_out <- ks.test(a, b)[[1]]
    expect_equivalent(base_out, abs(out))
})

test_that("signed ks_test returns expected number",{
    expect_equal(out, -0.53)
})

out_same <- ks_test(a, a)

test_that("ks_test returns 0 for same distribution",{
    expect_equal(out_same, 0)
})