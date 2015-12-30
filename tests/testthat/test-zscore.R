context("test zscore")

set.seed(12321)

x <- rnorm(100)

zscore_out <- zscore(x)
scale_out <- scale(x)[,1]

test_that("zscore returns numeric vector",{{
    expect_is(zscore_out, 'numeric')
    expect_true(length(zscore_out) == 100)
}})

test_that("zscore returns same result as scale",{
    expect_equivalent(zscore_out, scale_out)
})
