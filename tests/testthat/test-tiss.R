context("tiss works")

data(ex_data)

metadata <- construct_metadata(ex_data,
                               compound_col = 'Metadata_compound',
                               conc_col = 'Metadata_concentration',
                               feature_cols = 2:68,
                               negative_control = "DMSO")

tiss_out <- tiss(data = exdata, metadata = metadata)
tiss_unaligned <- tiss(data = ex_data, metadata = metadata, align = FALSE)

test_that("tiss works as expected",{
	expect_true(class(tiss_out) == 'dist')
	expect_true(class(tiss_unaligned) == 'dist')
})