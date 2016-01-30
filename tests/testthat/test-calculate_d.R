context("test calculate_d")

data(ex_data)

metadata <- construct_metadata(ex_data,
                               compound_col = 'Metadata_compound',
                               conc_col = 'Metadata_concentration',
                               feature_cols = 2:68,
                               negative_control = "DMSO")


compound_data <- get_compound_data(ex_data, metadata)

negative_control <- get_negative_control(ex_data, metadata)

out <- calculate_d(compound_data, negative_control)

test_that("calculate_d returns errors when expected",{
    expect_error(calculate_d("string", "string"))
    expect_error(calculate_d(1, 2))
    expect_error(calculate_d(compound_data, c(1,2,3)))
    expect_error(calculate_d(c(1,2,3), negative_control))
})

test_that("calculate_d returns expected values",{
    expect_is(out, 'matrix')
    expect_equal(dim(out), c(268, 10))
    expect_equal(colnames(out),
                 as.vector(metadata$compounds[order(metadata$compounds)]))
})