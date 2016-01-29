context("test loo_cor() works as expected")

test_that("returns errors",{
	expect_error(loo_cor("string"))
	expect_error(loo_cor(iris[, 1:4], 3))
	expect_error(loo_cor(c(1,2,3), 1))
})

test_that("returns expected output",{
	out_row <- loo_cor(iris[, 1:4])
	expect_equal(length(out_row), 150)

	out_col <- loo_cor(iris[, 1:4], 2)
	expect_equal(length(out_col), 4)

	expect_is(out_row, 'numeric')

	expected <- c(0.894, -0.348,  0.863,  0.921)
	expect_equal(out_col, expected, tolerance = 1e-3)
})

