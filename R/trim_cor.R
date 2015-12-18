#' Calculates correlation of shifted vectors
#' 
#' This function is for calculating the correlation between two vectors \code{x}
#' and \code{y}. The vectors are shifted \code{n} times by \code{p} elements. 
#' This relates to shifted n number of concentrations in either direction, each 
#' concentration shift being a multiple of the number of features.
#' 
#' @param x vector
#' @param y vector
#' @param n integer. The number of titrations shifts in either directions. i.e 
#'   \code{n = 2} will shift 2 titrations to the left, and 2 to the right.
#' @param p interger. The number of features. \code{n} will always be a multiple
#'   of \code{p}. This is so identical features are always aligned despite
#'   titration shifts.
#'
#' @return cor_measurements vector of correlations for each shift
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
#' plot(out, type = "l")
#' 
#' # plot all correlations
#' out <- trim_cor(df[,1], df[,2], 20, 3)
#' plot(out,
#'      type = "l",
#'      xaxt =  "n",
#'      ylab = "correlation",
#'      main = "col_1 vs all correlation",
#'      sub = "20 shifts, 3 elements each")
#' axis(1, at = 1:41,
#'      labels = names(out))
#' for (i in 3:ncol(df)){
#'  lines(trim_cor(df[,1], df[,i], 20, 3), col = "gray50")
#' }

trim_cor <- function(x, y, n, p){

  cor_measurements <- vector(length = 2 * n + 1)
  
  # left shift from -n to -1
  for (i in n:1){
    x_tmp <- tail(x, - i * p)
    y_tmp <- head(y, - i * p)
    cor_measurements[i] <- cor(x_tmp, y_tmp)
  }
  # reverse measurements so that they are in the correct order
  # going from -n -> 0 -> +n
  cor_measurements[1:n] <- rev(cor_measurements[1:n])
  
  # cor of unshifted x and y
  cor_measurements[n+1] <- cor(x, y)
  
  # right shift from 1 to n
  for (i in 1:n){
    x_tmp <- head(x, - i * p)
    y_tmp <- tail(y, -i * p)
    cor_measurements[n + 1 + i] <- cor(x_tmp, y_tmp)
  }
  
  # name of shift in vector of correlation values
  names(cor_measurements)[1:n] <- paste("-", n:1)
  names(cor_measurements)[n + 1] <- 0
  names(cor_measurements)[(n + 2):length(cor_measurements)] <- paste("+", 1:n)
  return(cor_measurements)
  
}