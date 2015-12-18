#' plot results from trim_cor
#' 
#' Given a vector of correlation values from \code{trim_cor}, this will produce
#' a line graph showing correlation with +/- shifts
#' 
#' @param x a vector of correaltion values from \code{trim_cor}
#' @param ... arguments to be passed to plot
#' 
#' @return graph to the device
#' 
#' @export
#' 
#' @examples
#' 
#' # create dummy dataset
#' l <- list()
#' for (i in 1:100) l[[i]] <- rnorm(100)
#' df <- as.data.frame(l)
#' names(df) <- paste("col", 1:100, sep = "_")
#' # order values (more obvious correlation shifts)
#' df <- apply(df, 2, sort)
#' out <- trim_cor(df[,1], df[,2], n = 10, p = 5)
#' plot_trim_cor(out, main = "plot of correlation values")

plot_trim_cor <- function(x, ...){
  stopifnot(class(x) == "cor_vector")
  plot(x,
       type = "l",
       xaxt = "n",
       ylab = "correlation",
       ...)
  axis(1,
       at = 1:length(names(x)),
       labels = names(x))
}
