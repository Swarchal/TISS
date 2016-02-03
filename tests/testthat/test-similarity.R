context("test similarity")

test_that("euclid distance works",{
	x_same <- euclid_dist(c(1,2), c(1,2))
	x_dist <- euclid_dist(c(1,1), c(2,2))

	expect_equal(x_same, 0)
})

test_that("expand_grid_unique works",{
	a <- c(1,2,3)
	b <- c('a', 'b', 'c')
	out <- expand_grid_unique(a, b)
	out_false <- expand_grid_unique(a, a, include.equals = TRUE)

	expect_equal(dim(out), c(9, 2))
	expect_equal(
		dim(expand_grid_unique(c(1,2,3), c(1,2,3), include.equals = TRUE)),
		c(6,2))

	expect_equal(
		dim(expand_grid_unique(c(1,2,3), c(1,2,3), include.equals = FALSE)),
		c(3,2))
})



data(ex_data)
metadata <- construct_metadata(ex_data,
                               compound_col = 'Metadata_compound',
                               conc_col = 'Metadata_concentration',
                               feature_cols = 2:68,
                               negative_control = "DMSO")
compound_data <- get_compound_data(ex_data, metadata)
negative_control <- get_negative_control(ex_data, metadata)
d_out <- calculate_d(compound_data, negative_control)
d_scale <- scale_d(d_out)
out <- correlate(d_scale, metadata)
out_full <- correlate(d_scale, metadata, return_max = FALSE)
ans <- trim(d_scale, out, metadata = metadata)
sim_list <- similarity_list(ans)

test_that("similarity_list works",{
	expect_true(class(sim_list) == 'dist')
	diag_out <- as.numeric(diag(as.matrix(sim_list)))
	expect_equal(diag_out, rep(0, 10))
})

test_that("similarity_df works",{
	sim_df <- similarity_df(d_scale)
	expect_true(class(sim_df) == 'dist')
})