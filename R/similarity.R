#' euclidean distance between two vectors
#'
#' as \code{dist()} has stupid defaults
#'
#' @param a numeric vector.
#' @param b numeric vector
#' @param ... additional arguments to pass to \code{dist}
#' @return Euclidean distance between the two vectors

euclid_dist <- function(a, b,...){
    dist(rbind(a, b),...)[1]
}


#' Unique combination of two vectors
#'
#' Given two vectors will find all unique pairs.
#'
#' @param x Vector
#' @param y Vector
#' @param include.equals if FALSE will not return a pair that is the same
#' element paired against itself
#'
#' @return dataframe
#'
#' @examples
#' x <- c('one', 'two', 'three')
#' y <- c('a', 'b', 'c')
#' 
#' expand_grid_unique(x, y)

expand_grid_unique <- function(x, y, include.equals = TRUE){
    x <- unique(x)
    y <- unique(y)
    
    g <- function(i){
        z <- setdiff(y, x[seq_len(i - include.equals)])
        if(length(z)) cbind(x[i], z, deparse.level = 0)
    }
    
    do.call(rbind, lapply(seq_along(x), g))
}



#' similarity score
#' 
#' Essentially a pair-wise euclidean distance between each vector in the list
#' 
#' @param x dataframe of compound vectors, with a compound vector per column
#' @param return_matrix if TRUE will return a distance matrix, otherwise a
#'    dataframe will be returns
#' @param ... additional arguments to pass to distance function, see \code{dist}
#' @return distance matrix or dataframe, dependent on \code{return_matrix}
#' 
#' @export

similarity_df <- function(x, return_matrix = TRUE, ...){

    pairs <- expand_grid_unique(colnames(x), colnames(x), include.equals = TRUE)
    
    
    # create dataframe to store values
    out <- data.frame(A = c(rep(NA, nrow(pairs))),
                      B = c(rep(NA, nrow(pairs))),
                      dist = rep(NA, nrow(pairs)))
    
    for (row in 1:nrow(pairs)){
        x_name <- pairs[row, 1]
        y_name <- pairs[row, 2]
        out$A[row] <- x_name
        out$B[row] <- y_name
        out$dist[row] <- euclid_dist(x[, x_name], x[, y_name],...)
    }
    
    # convert to distance matrix
    if (return_matrix){
        out <- as.dist(xtabs(out[, 3] ~ out[, 2] + out[, 1]), diag = TRUE)
    }
    
    return(out)
}


#' similarity score
#' 
#' Essentially a pair-wise euclidean distance between each vector in the list
#' 
#' @param x list of compound vectors produced by \code{trim()}
#' @param return_matrix if TRUE will return a distance matrix, otherwise a
#'    dataframe will be returns
#' @param ... additional arguments to pass to distance function, see \code{dist}
#' @return distance matrix or dataframe, dependent on \code{return_matrix}
#' 
#' @export

similarity_list <- function(x, return_matrix = TRUE, ...){
    
    pairs <- expand_grid_unique(names(x), names(x), include.equals = TRUE)
    
    # create dataframe to store values
    out <- data.frame(A = c(rep(NA, nrow(pairs))),
                      B = c(rep(NA, nrow(pairs))),
                      dist = rep(NA, nrow(pairs)))
    
    for (row in 1:nrow(pairs)){
        x_name <- pairs[row, 1]
        y_name <- pairs[row, 2]
        out$A[row] <- x_name
        out$B[row] <- y_name
        out$dist[row] <- euclid_dist(x[[x_name]], x[[y_name]],...)
    }
    
    # convert to distance matrix
    if (return_matrix){
        out <- as.dist(xtabs(out[, 3] ~ out[, 2] + out[, 1]), diag = TRUE)
    }
    
    return(out)
    
}