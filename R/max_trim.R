#' Converts output from max_shift() to a shifted vector

#' Trims a vector from the results of max_shift
#'
#' Given a vector, this function will trim it according to the output from
#' \code{max_trim_single}. \code{max_trim_single} will trim directly the number
#' of elements specified by \code{max_shift}, with no input from the number of
#' features. In order to trim the vector by the number of features present
#' \code{max_trim} should be used instead of \code{max_trim_single}.
#'
#' @param vector Vector to be trimmed
#'
#' @param max_shift Character containing direction and number of elements to
#' 		trim in that direction, or '0' for no trim. e.g \code{'+2'}. 
#' 
#' @return vector_out The trimmed vector
#'
#' @export
#'
#' @examples
#' # example data
#' v <- seq(1, 10, 1)
#'
#' max_trim_single(v, '+2')
#' max_trim_single(v, '-3')
#' max_trim_single(v, '0')


max_trim_single <- function(vector, max_shift){
    
    signs <- c('+', '-')
    
    # check inputs
    if (!is.character(max_shift)){
        stop("max_shift needs to a character")
    }
    # check the shift has a sign or is otherwise '0'
    if (length(unlist(strsplit(max_shift, ""))) == 1 && max_shift != '0'){
        stop("shift needs to be either '0' or contain a sign (+/-)")
    }
    if (!is.vector(vector)){
        stop("vector needs to be a vector ...")
    }
    if (length(vector) < 2){
        stop("vector needs to be at least two elements")
    }
    
    # if vector is just 0, then return the vecto as-is without trimming
    if (max_shift == '0'){
        vector_out <- vector
        
        # split max_shift into individual characters
    } else {
        split_shift <- unlist(strsplit(max_shift, signs))
        
        # convert character numbers into numeric
        # TODO: cleaner way of doing this
        numbers <- as.numeric(paste0(
            split_shift[2:length(split_shift)], collapse = ""))
            
            if (split_shift[1] == '+'){
                vector_out <- head(vector, - numbers)
            } else if (split_shift[1] == '-'){
                vector_out <- tail(vector, - numbers)
            } else stop("max_shift should start with a sign (+/-) or be '0'")
            
    }
    return(vector_out)
}


#' Converts output from max_shift() to a shifted vector

#' Trims a vector from the results of max_shift.
#'
#' Given a vector, this function will trim it according to the output from
#' \code{max_trim}. That is will be trimmed to maximise correlation with the
#' rest of the vectors. \code{max_trim} will trim the vectors by a multiple
#' of the number of features extracted from \code{metadata}.
#'
#' @param vector Vector to be trimmed
#' @param max_shift Character containing direction and number of elements to
#'      trim in that direction, or '0' for no trim. e.g \code{'+2'}. 
#' @param metadata Metadata object to extract the number of features.
#'   This should be created through \code{construct_metadata}.
#' @return vector_out The trimmed vector
#'
#' @export


max_trim <- function(vector, max_shift, metadata){
    
    signs <- c('+', '-')

    if (class(metadata) != 'metadata'){
        stop("metadata needs to be a metadata object")
    }
    
    # check inputs
    if (!is.character(max_shift)){
        stop("max_shift needs to a character")
    }
    # check the shift has a sign or is otherwise '0'
    if (length(unlist(strsplit(max_shift, ""))) == 1 && max_shift != '0'){
        stop("shift needs to be either '0' or contain a sign (+/-)")
    }
    if (!is.vector(vector)){
        stop("vector needs to be a vector ...")
    }
    if (length(vector) < 2){
        stop("vector needs to be at least two elements")
    }

    n_features <- length(metadata$feature_cols)
    
    # if vector is just 0, then return the vecto as-is without trimming
    if (max_shift == '0'){
        vector_out <- vector
        
        # split max_shift into individual characters
    } else {
        split_shift <- unlist(strsplit(max_shift, signs))
        
        # convert character numbers into numeric
        # TODO: cleaner way of doing this
        numbers <- as.numeric(paste0(
            split_shift[2:length(split_shift)], collapse = ""))
            
            if (split_shift[1] == '+'){
                vector_out <- head(vector, - (n_features * numbers))
            } else if (split_shift[1] == '-'){
                vector_out <- tail(vector, - (n_features * numbers))
            } else stop("max_shift should start with a sign (+/-) or be '0'")
            
    }
    return(vector_out)
}