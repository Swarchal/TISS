#' Creates dataframe of negative control featuredata
#' 
#' Given a dataframe and a metadata list, this function will subset the
#' dataframe and return only the featuredata for compounds matching the name
#' of the negative control
#' 
#' @param df dataframe
#' @param metadata list
#' 
#' @return df_subset dataframe
#' 
#' @export

get_negative_control <- function(df, metadata){
    
    if(!is.data.frame(df)) stop("df has to be a dataframe")
    if(!is.list(metadata)) stop("metadata has to be a list")
    
    df_subset <- df[metadata$feature_cols,
                    metadata$compounds == metadata$negative_control]
    
    return(df_subset)
}