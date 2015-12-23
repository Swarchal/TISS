#' Find the shift with the best correlation
#'
#' For a number of correlation meansurements produced by \code{trim_cor}
#' will return the shift with the highest correlation value.
#'
#' @param cor_measurements vector of correlation values and shift
#' @param value boolean, whether to return a numerical correlation value
#' @return max_shift shift with the highest correlation. If \code{value} is
#'		\code{TRUE} then it will also return the correlation value
#'
#' @export
#'
#' @examples
#' # create dummy dataset
#' l <- list()
#' for (i in 1:100) l[[i]] <- rnorm(100)
#' df <- as.data.frame(l)
#' names(df) <- paste("col", 1:100, sep = "_")
#' # order values (more obvious correlation shifts)
#' df <- apply(df, 2, sort)
#' out <- trim_cor(df[,1], df[,2], n = 5, p = 5)
#'
#' max_shift(out)
#' max_shift(out, value = TRUE)

max_shift <- function(cor_measurements, value = FALSE){
    
    # check cor_measurements is the correct class
    if (class(cor_measurements) != "cor_vector"){
        stop("input needs to have a class of 'cor_vector'")
    }
    
    # find the maxmimum correlation value
    # Can't use max() as it removes attributes
    max_shift <- sort(cor_measurements, decreasing = TRUE)[1]
    
    if (value == FALSE) return(names(max_shift))
    else if (value == TRUE) return(max_shift)
    else stop("'value' has to be either TRUE or FALSE")
    
}