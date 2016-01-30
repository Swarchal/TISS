context("testing ability to parse metadata")

data(ex_data)

metadata <- construct_metadata(ex_data,
                               compound_col = 'Metadata_compound',
                               conc_col = 'Metadata_concentration',
                               feature_cols = 2:68,
                               negative_control = "DMSO")


compound_data <- get_compound_data(ex_data, metadata)

negative_control <- get_negative_control(ex_data, metadata)

test_that("get_compound_data returns errors when expected",{
    expect_error(get_compound_data(ex_data, c(1, 2, 4)))
    expect_error(get_compound_data(ex_data[, 1], metadata))
    expect_error(get_compound_data(c(1,2,3), metadata))
})

test_that("get_compound_data returns expected values",{
    expect_warning(get_compound_data(ex_data, metadata))
    expect_is(compound_data, 'list')
    expect_equal(length(compound_data), 10)
    expect_equal(length(compound_data[[1]][[1]]), length(metadata$feature_cols))
})

test_that("get_negative_control returns errors when expected",{
    expect_error(get_negative_control(ex_data, c(1,2,3)))
    expect_error(get_negative_control(c(1,2,3), metadata))
})

test_that("get_negative_control returns expected values",{
    expect_is(negative_control, 'data.frame')
    expect_equal(ncol(negative_control), length(metadata$feature_cols))
})