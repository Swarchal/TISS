context("test correlate")

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

test_that("correlate returns errors when expected",{
    expect_error(correlate("string", "string"))
    expect_error(correlate(d_scale, list(iris)))
    expect_error(correlate(c(1,2,3), metadata))
})

test_that("correlate returns expected values",{
    expect_true(class(out) == 'list')
    expect_is(class(out[[1]]), 'character')
    expect_equal(length(out), length(metadata$compounds))
    expect_equal(names(out), colnames(d_scale))
    expect_equal(length(out[[1]]), 1)
    expect_equal(length(out_full[[1]]), 3)
})