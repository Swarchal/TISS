context("max_trim checks")

# example data
v <- seq(1, 10, 1)

out  <- max_trim(v, '+2')
out1 <- max_trim(v, '-3')
out2 <- max_trim(v, '0')

test_that("max_trim returns a number",{
    expect_is(out, "numeric")
    expect_true(is.vector(out))
})

test_that("max trim returns expected answer",{
    ans = seq(1, 8, 1)
    expect_equal(out, ans)
})

test_that("max_trim(x, 0) returns x",{
    expect_equal(max_trim(v, '0'), v)
})