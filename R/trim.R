#' Trim compound vectors
#'
#' Given a scaled compound vector and list of shifts that maximise correlation
#' \code{trim} will trim each compound vector according to those shifts. i.e
#' they will be trimmed by n titrations worth of features, where n is the shift
#'
#' @param x a scaled compound vector produced from \code{scale_d()}
#' @param max a list of maximum shifts, produced by the output from
#'   \code{correlate} and \code{max_shift}
#' @return a list of trimmed compound vectors
#'
#' @export
#'

trim <- function(x, max){

	# initialise list for results and set names
	out_trimmed <- vector('list', length = ncol(x))
	names(out_trimmed) <- colnames(x)

	# check compound names are the same and in the same order
	for (i in 1:ncol(x)){
		if (colnames(x)[i] != names(max)[i]){
			stop("Compounds names do not match between 'x' and 'max'")
		}
	}

	# trim vector in x according to result from max
	for (i in 1:ncol(x)){
		out_trimmed[[i]] <- max_trim(x[, i], max[[i]])
	}

	return(out_trimmed)
}