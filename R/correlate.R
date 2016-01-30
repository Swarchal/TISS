#' Internal function used in correlate()
#'
#' \code{trim_cor} should be used interactively.
#'
#' @param x First cor_vector
#' @param y Second vector
#' @param n Number of shifts in either direction
#' @param p Number of features per shift
#' @return cor_measurements


# bare-bones trim_cor function
trim_cor_ <- function(x, y, n, p){
	
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
	cor_measurements[n + 1] <- cor(x, y)
	
	# right shift from 1 to n
	for (i in 1:n){
	    x_tmp <- head(x, - i * p)
	    y_tmp <- tail(y, - i * p)
	    cor_measurements[n + 1 + i] <- cor(x_tmp, y_tmp)
	}
	
	# name of shift in vector of correlation values
	names(cor_measurements)[1:n] <- paste("-", n:1)
	names(cor_measurements)[n + 1] <- 0
	names(cor_measurements)[(n + 2):length(cor_measurements)] <- paste('+', 1:n)

	# create class for plot_trim_cor
	class(cor_measurements) <- "cor_vector"
	
	return(cor_measurements)
	
}


#' Calculates correlation between vectors shifted by titrations.
#'
#' For a matrix, with each row representing a compound vector constructed of
#' features and titrations. \code{correlate} will calculate the correlation 
#' for each vector compared against the average of all the other vectors,
#' shifted by a number of titrations in both directions. This is to align
#' compound vectors invariant of differing potencies.
#'
#' @param x Matrix or dataframe of compound vectors. Each column representing
#'    an individual compound vector. This should have been produced by 
#' @param metadata corresponding metadata for x, produced by \code{calculate_d}
#'   and scaled with \code{scale_d}
#'   \code{construct_metadata}
#' @return List of correlation measurements, each element of the list
#'   corresponds to a compound vector in the order given by \code{x}
#'
#' @export

correlate <- function(x, metadata){

	if (class(metadata) != 'metadata'){
		stop("Metadata has to be a metadata object calculated by 'construct_metadata()'")
	}

	n <- as.integer(length(metadata$concentrations) / 2)
	p <- length(metadata$feature_cols)

	# 2. initialise somewhere to store optimal shifts for each col
	out_list <- vector('list', length = ncol(x))

	# 3. main loop for each column in x
	for (col in 1:ncol(x)){

		tmp <- as.numeric(x[, col])
		rest <- x[, -col]
		av_rest <- as.numeric(rowMeans(rest))
		out_list[[col]] <- trim_cor_(
			x = tmp,
			y = av_rest,
			n = n,
			p = p)
	}

	# name elements of list with compound names
	names(out_list) <- colnames(x)
	return(out_list)
}