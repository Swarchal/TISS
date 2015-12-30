context("Trim correlations")

set.seed(12321)
a <- rnorm(100)
b <- rnorm(100)
out <- trim_cor(a, b, 5, 5)

test_that("Returns a cor_vector",{
    expect_is(out, "cor_vector")
})

test_that("cor_vector is expected length",{
    expect_equivalent(length(out), 11)
})

test_that("cor of 1 for same unshifted vector",{
    out_same <- trim_cor(a, a, 5, 5)[[6]]
    expect_equal(out_same, 1)
})
