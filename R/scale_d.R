#' Z-score normalises D-statistic vectors
#' 
#' Given a table of D-statistic values calculated by \code{calculate_d}. This 
#' function normalises each column via a z-score. With column representing the 
#' D-statistic vector for a compound. Each column is normalised independently of
#' the other columns in d_table
#' 
#' @param d_table matrix or dataframe of D-statistics
#'   
#' @return z_out matrix or dataframe of z-scored D-statistics
#'
#' @export

scale_d <- function(d_table){
    
    if (!is.matrix(d_table) && !is.data.frame(d_table)){
        stop("d_table needs to be a matrix or a dataframe")
    }
    
    z_out <- apply(d_table, 2, zscore)
    return(z_out)
}