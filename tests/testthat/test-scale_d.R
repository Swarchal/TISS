context("function test for scale_d")

set.seed(12321)

df <- data.frame(
    a = rnorm(100),
    b = rnorm(100),
    c = rnorm(100))

out <- scale_d(df)
out_mat <- scale_d(as.matrix(df))

test_that("scale_d only accepts dataframe or matrix",{
    expect_error(scale_d("string"))
    expect_is(out_mat, 'matrix')
    expect_is(out, 'matrix')
})


test_that("scale_d scales each column individually",{
    out_mean <- apply(out, 2, mean)
    out_mean_r <- round(out_mean, 3)
    expect_equivalent(out_mean_r, c(0, 0, 0))
})