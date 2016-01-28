context("Trim correlations")

set.seed(12321)
a <- rnorm(100)
b <- rnorm(100)
out <- trim_cor(a, b, 5, 5)

test_that("Returns a cor_vector",{
    expect_is(out, "cor_vector")
})

test_that("cor_vector is expected length",{
    expect_equivalent(length(out), 11)
})

test_that("cor of 1 for same unshifted vector",{
    out_same <- trim_cor(a, a, 5, 5)[[6]]
    expect_equal(out_same, 1)
})

# example metadata
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

test_that('trim cor can use values from metadata',{

    expect_is(trim_cor(x = a, y = b, metadata = metadata), 'cor_vector')
})