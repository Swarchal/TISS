context("testing trim()")

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

ans <- trim(d_scale, out)

test_that("trim returns expected values",{
    expect_equal(length(ans), length(metadata$compounds))
    expect_true(class(ans) == 'list')
    expect_true(class(ans[[1]]) == 'numeric')
    expect_equal(names(ans), colnames(d_scale))
    expect_error(trim(d_scale, out_full))
})