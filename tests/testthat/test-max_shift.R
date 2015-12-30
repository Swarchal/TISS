context("max_shift and trim_corworking as expected")

set.seed(12321)

l <- list()
for (i in 1:100) l[[i]] <- rnorm(100)
df <- as.data.frame(l)
names(df) <- paste("col", 1:100, sep = "_")
# order values (more obvious correlation shifts)
df <- apply(df, 2, sort)
out <- trim_cor(df[,1], df[,2], n = 5, p = 5)

out1 <- max_shift(out)
out2 <- max_shift(out, value = TRUE)

test_that("max_trim only accepts class of cor_vector",{
    expect_is(out, 'cor_vector')
    expect_error(max_shift(df))
})

test_that("max_shift returns character",{
    expect_true(is.character(out1))
})

test_that("max_shift returns expected answer",{
    expect_equal(out1, '0')
    expect_is(out2, 'numeric')
})

