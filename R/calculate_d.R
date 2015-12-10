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
#' @param z_score boolean, whether to normalise compound

calculate_d <- function(cmpd, neg, vectorise = TRUE, z_score = FALSE){
    # check input    
    if (!is.data.frame(neg)) stop("neg needs to be a dataframe")
    if (!is.list(cmpd)) stop("cmpd needs to be a list of dataframes")
    
    if (vectorise == FALSE & z_score == TRUE){
        stop(paste("Cannot normalise compound vectors if 'vectorise is'", vectorise))
    }
    
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
    if (total_ncols < numeric_ncol) {
        stop("All columns need to be numeric")
    }
    
    # should give a vector of values for each compound-concentration
        # ks_test across pairs of columns in two dataframes
        # holding the standard dataframe constant as neg
    D_values <- lapply(cmpd, ks_cols, g = neg)
    
    # TODO: concatenate the concentration values for each compound
        # leaving a D-vector for each compound
        # - check if sapply gives a more sensible output than lapply
}