#' Signed Kolmogorov-Smirnov Test
#' 
#' A two-sample Kolmogorov-Smirnov test that also returns the direction of the
#' calculated supremum relative to the distribution g. Used within \code{ks_cols}
#' For example, if the values of g are smaller than g, and the CDF of f is
#' shift to the left of g, then the D statistic will be negative, if it is
#' shift to the right of g, then it will be positive.
#' 
#' @param f Numerical vector to be compared to g
#' @param g Numerical vector, reference or control
#' 
#' @return D D-statistic
#' 
#' @examples 
#' data(iris)
#' ks_test(iris[,1], iris[,2])
#' 
#' @export


ks_test <- function(f, g){
    
    if (length(f) && length(g) > 0){
        bin_edges <- sort(c(f, g))
        
        bin_f <- hist(f, breaks = bin_edges, plot = FALSE)$counts
        bin_g <- hist(g, breaks = bin_edges, plot = FALSE)$counts
        
        sum_counts_f <- cumsum(bin_f) / sum(bin_f)
        sum_counts_g <- cumsum(bin_g) / sum(bin_g)
        
        sample_ecdf_f <- sum_counts_f[1:length(sum_counts_f) - 1]
        sample_ecdf_g <- sum_counts_g[1:length(sum_counts_g) - 1]
        
        upDeltaCDF   <- sample_ecdf_f - sample_ecdf_g
        downDeltaCDF <- sample_ecdf_g - sample_ecdf_f
        
        up <- max(upDeltaCDF)
        down <- max(downDeltaCDF)
        
        if (up >= down){
            D <- up
        } else if (up < down){
            D <- - down
        }
    } else D <- 0
    
    return(D)
}
