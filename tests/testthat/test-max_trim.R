context("max_trim checks")

# example data
v <- seq(1, 10, 1)

out  <- max_trim_single(v, '+2')
out1 <- max_trim_single(v, '-3')
out2 <- max_trim_single(v, '0')

test_that("max_trim_single returns a number",{
    expect_is(out, "numeric")
    expect_true(is.vector(out))
})

test_that("max trim_single returns expected answer",{
    ans = seq(1, 8, 1)
    expect_equal(out, ans)
})

test_that("max_trim_single(x, 0) returns x",{
    expect_equal(max_trim_single(v, '0'), v)
})

test_that("max_trim_single returns meaningful errors",{
    expect_error(max_trim_single(v, 1))
    expect_error(max_trim_single(v, "1"))
    expect_error(max_trim_single(4, '+1'))
    expect_error(max_trim_single(list(v), '+1'))
})