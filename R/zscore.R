#' Z-score of KS values
#' 
#' Used within \code{calculate_d}
#' Scale and center the D statistic produced by \code{ks_test}
#'
#' @param x Vector of D values
#' 
#' @return out Vector of z-scored values
#' 
#' @export

zscore <- function(x){
    out <- scale(x)
    attributes(out) <- NULL
    return(out)
}
