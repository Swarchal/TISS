#' Leave one out correlation
#'
#' A way to calculate the correlation between multiple vectors. This is a
#' quicker method that compares the correlation between a single vector any
#' the average for all the others.
#'
#' @param x matrix or dataframe of numerical vectors
#' @param i margin over which to divide the vectors. For example, if 1 is given
#'   then this will separate the vectors per each row, if 2 is given, then the 
#'   vectors will be separated column-wise
#'
#' @return out vector of correlation values, each value
#'
#' @export
#' @examples
#' # correlation between rows of iris
#' loo_cor(iris[, 1:4])
#' # correlation between columns of iris
#' loo_cor(iris[, 1:4], 2)


loo_cor <- function(x, i = 1){
    
    # check input is dataframe or matrix
    if (!is.data.frame(x) && !is.matrix(x)){
        stop("'x' needs to be either a matrix or a dataframe")
    }
    
    # check to see if i is either 1 or 2
    accepted_is <- c(1, 2)
    if (! i %in% accepted_is){
        stop("'i' needs to be either 1 or 2")
    }
    
    # TODO error message for non-numeric values
    
    if (i == 1){
        # row-wise
        out <- vector(length = nrow(x))
        for (row in 1:nrow(x)){
            tmp <- as.numeric(x[row, ])
            rest <- x[-(row), ]
            av_rest <- as.numeric(colMeans(rest))
            out[row] <- cor(tmp, av_rest)
        }
    }
    
    if (i == 2){
        # column-wise
        out <- vector(length = ncol(x))
        for (col in 1:ncol(x)){
            tmp <- as.numeric(x[, col])
            rest <- x[, -(col)]
            av_rest <- as.numeric(rowMeans(rest))
            out[col] <- cor(tmp, av_rest)
        }
    }
    return(out)
}