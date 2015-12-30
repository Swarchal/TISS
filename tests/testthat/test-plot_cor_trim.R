context("test plot_trim_cor")

set.seed(12321)
l <- list()
for (i in 1:100) l[[i]] <- rnorm(100)
df <- as.data.frame(l)
names(df) <- paste("col", 1:100, sep = "_")
# order values (more obvious correlation shifts)
df <- apply(df, 2, sort)
out <- trim_cor(df[,1], df[,2], n = 10, p = 5)
plt_out <- plot_trim_cor(out, main = "plot of correlation values")

test_that("plt_out only accepts cor_vector class",{
	expect_error(plot_trim_cor(df))
})