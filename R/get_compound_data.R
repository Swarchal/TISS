#' Subset compound data
#' 
#' Creates a list of list of dataframes containing only feature data.
#' First splits df into a list, with each element containing a compound.
#' Then, splits each of the compound lists into a list of dataframes, with a 
#' dataframe corresponding to each compound-concentration pair.
#' 
#' @param df dataframe
#' @param metadata list
#' 
#' @return split_featuredata list
#' 
#' @export

get_compound_data <- function(df, metadata){
    
    if(!is.data.frame(df)) stop("df has to be a dataframe")
    if(!is.list(metadata)) stop("metadata has to be a list")
    
    # construct list of list of dataframes
    
    # list of compounds
    split_by_compound <- split(df, metadata$compounds)
    # each element 'compound' contains list of dataframes for each concentration
    split_all <- lapply(split_by_compound, split, f = metadata$concentrations)
    
    # subset featuredata
    split_featuredata <- lapply(split_all, '[', metadata$feature_cols)
    
    return(split_featuredata)
}