#' Correlation between two vectors shifted by n
#' 
#' Measures the correlation between two vectors shifted by 'n' elements. For
#' example, \code{cor_trim(a, b, n = 5, shift = 5)} will return the correlation
#' between the vectors a and b, with the last 5 elements of a removed, and the 
#' first 5 elements of b removed
#' 
#' @param a vector
#' @param b vector
#' @param n number of elements to shift the vector by
#' @param string, the direction to shift the first vector in.
#' 
#' @return cor_out the correlation
#' 
#' @examples
#' a <- rnorm(100)
#' b <- rnorm(100)
#' cor_trim(a, b, n = 5)
#' cor_trim(a, b, n = 5, shift = "right")
#' 
#' @export

cor_trim <- function(a, b, n = 0, shift = "left"){
  
  n <- abs(n)
  
  if (n == 0){
    cor_out <- cor(a, b)
    
  } else if(shift == "left"){
    a <- head(a, -n)
    b <- tail(b, -n)
    
  } else if (shift == "right"){
    a <- tail(a, -n)
    b <- head(b, -n)
  } else stop("shift has to be left or right")
  
  cor_out <- cor(a, b)
  return(cor_out)
}