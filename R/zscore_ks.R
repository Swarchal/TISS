#' Z-score of KS values
#' 
#' Scale and center the D statistic produced by \code{ks_test}
#'
#' @param x Vector of D values
#' 
#' @return out Vector of z-scored values
#' 
#' @export

zscore_ks <- function(x){
    out <- scale(x)
    return(out)
}