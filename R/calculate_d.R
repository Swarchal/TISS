#' Calculates a vector of KS values for each compound
#' 
#' Given a dataframe for the negative control and a list of dataframes for each 
#' compound and concentration, this will calculate a signed D-statistic from a 
#' KS test for each feature (per compound and concentration). If 
#' \code{vectorise} is TRUE, then will collapse the concentration vectors and 
#' return a single vector per compound. If \code{z_score} is true, then the
#' returned values will be normalised per compound into z-scores.
#' 
#' @param cmpd list
#' @param neg dataframe or matrix
#' @param vectorise boolean, whether to return a vector per compound
#'
#' @export

calculate_d <- function(cmpd, neg, vectorise = TRUE){
    # check input    
    if (!is.data.frame(neg)) stop("neg needs to be a dataframe")
    if (!is.list(cmpd)) stop("cmpd needs to be a list of dataframes")
    
    # function to check if columns are numeric
    col_numeric <- function(x) sapply(x, is.numeric)
    
    # check number of numeric columns
    neg_numeric_columns <- sum(col_numeric(neg))
    if (neg_numeric_columns < ncol(neg)){ 
        stop("All columns need to be numeric")
    }
    
    # check number of numeric columns
    total_ncols <- sum(Reduce('+', lapply(cmpd, ncol)))
    numeric_ncols <- sum(Reduce('+', lapply(cmpd, col_numeric)))
    if (total_ncols < numeric_ncols) {
        stop("All columns need to be numeric")
    }
    
    # should give a vector of values for each compound-concentration
    D_values <- sapply(cmpd, function(x) lapply(x, ks_cols, g = neg))
    
    if (vectorise == TRUE){
        D_values <- apply(d_out, 2, unlist)
    }
    
    return(D_values)
}