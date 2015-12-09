#' Signed KS test across pairs of columns
#' 
#' Given two numerical columns of matrices, this function will compute a signed
#' D-statistic for every pair of columns.
#' 
#' @param f dataframe or matrix of sample
#' @param g dataframe or matrix of control
#' 
#' @return tmp vector of D-statistics
#' 
#' @export

ks_cols <- function(f, g){
    
    # check inputs
    if (!is.matrix(f) && !is.data.frame(f)){
        stop("f needs to be a dataframe of a matrix")
    }
    if (!is.matrix(g) && !is.data.frame(g)){
        stop("g needs to be a dataframe of a matrix")
    }
    
    if (ncols(f) != ncols(g)){
        stop("Different number of columns in f and g")
    }
    
    tmp <- vector(length = ncol(g))
    for (i in 1:ncol(df_r)){
        tmp[i] <- ks_test(f[, i], g[, i])
    }
    return(tmp)
}