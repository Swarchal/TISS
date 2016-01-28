context("metadata is working properly")

# example dataset
concentration <- rep(c(100, 10, 1, 0.1), 10)
compounds <- sort(rep(letters[1:10], 4))
cell_area <- rnorm(40)
no_speckles <- rnorm(40, 100, 100)
df <- data.frame(compounds, concentration, cell_area, no_speckles)
metadata <- construct_metadata(
    df,
    compound_col = "compounds",
    conc_col = "concentration",
    feature_cols = 3:4,
    negative_control = "a")

test_that('returns object of correct type',{
    expect_is(metadata, 'metadata')    
})

test_that('output is as expected',{
    expect_equal(length(metadata$compounds), 9)
    expect_equal(metadata$negative_control, 'a')
    expect_equal(metadata$feature_cols, 3:4)
    expect_equal(length(metadata), 6)
})

test_that('returns errors when expected',{
    mat <- as.matrix(df)
    expect_error(
        metadata <- construct_metadata(
            mat,
            compound_col = "compounds",
            conc_col = "concentration",
            feature_cols = 3:4,
            negative_control = "a")
    )
    
    expect_error(metadata <- construct_metadata(
        df,
        compound_col = "compounds",
        conc_col = "concentration",
        feature_cols = c('cell_area', 'no_speckles'),
        negative_control = "a"))
})

test_that('returns warnings with concentration',{
    
    # introduce NAs into concentrations
    concentration <- rep(c(100, 10, 1, NA), 10)
    compounds <- sort(rep(letters[1:10], 4))
    cell_area <- rnorm(40)
    no_speckles <- rnorm(40, 100, 100)
    df <- data.frame(compounds, concentration, cell_area, no_speckles)
    
    expect_warning(construct_metadata(
        df,
        compound_col = "compounds",
        conc_col = "concentration",
        feature_cols = 3:4,
        negative_control = "a"))
})
    
test_that('returns warnings with compounds',{
    # introduce NaNs into compound list
    concentration <- rep(c(100, 10, 1, 0.1), 10)
    compounds <- c('a', rep(NA, 39))
    cell_area <- rnorm(40)
    no_speckles <- rnorm(40, 100, 100)
    df <- data.frame(compounds, concentration, cell_area, no_speckles)
    
    expect_warning(construct_metadata(
        df,
        compound_col = "compounds",
        conc_col = "concentration",
        feature_cols = 3:4,
        negative_control = "a"))
})