#' Titration invariant similarity score
#'
#' Calculates the TISS between compounds. The function is a wrapper
#' for a typical workflow. Though it's generally recommended to run
#' the functions separately for ease of debugging and sanity checks
#'
#' @param data input data in the form of a data.frame
#' @param metadata metadata object generated from /code{construct_metadata}
#' @param align whether to align compounds by maximum correlation in order
#'    to account fo differences in potency. If FALSE will not align the
#'    compounds and generate similarity scores between complete compound
#'    vectors
#'
#' @return similarity matrix or datarame if \code{return_matrix = FALSE}
#'    is passed as an additional argument
#'
#' @export

tiss <- function(data, metadata, align = TRUE){

	compound_data <- get_compound_data(data, metadata)
	negative_control <- get_negative_control(data, metadata)
	d_out <- calculate_d(compound_data, negative_control)
	d_scale <- scale_d(d_out)

	if (align == TRUE){
		out <- correlate(d_scale, metadata)
		ans <- trim(d_scale, out, metadata = metadata)
		sim_out <- similarity_list(ans)
	}

	if (align == FALSE){
		sim_out <- similarity_df(d_scale)
	}

	return(sim_out)

}