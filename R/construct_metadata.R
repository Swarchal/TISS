#' Constructs metadata list to be used in functions
#' 
#' Creates a list containing all necessary metadata to subset a dataframe
#' 
#' @param df Dataframe
#' @param compound_col Vector or dataframe column
#' @param conc_col Vector or dataframe column
#' @param feature_cols Vector
#' @param negative_control string
#' 
#' @return metadata list containing all metadata
#' 
#' @export


construct_metadata <- function(df, compound_col, conc_col, feature_cols, negative_control){
    
    # check input
    if(!is.data.frame(df)) stop("df needs to be a dataframe")
    if(!is.vector(compound_col)) stop("compounds_col needs to be a vector of compound names")
    if(!is.vector(conc_col)) stop("conc_col needs to be a vector of numeric concentration values")
    if(!is.numeric(feature_cols)) stop("feature_cols needs to be a numeric vector")
    if(!is.character(negative_control)) stop("negative_control needs to be a character string")
    
    # remove negative control from compound list
    all_compounds <- unique(df$compound_col)
    compounds <- all_compounds[! all_compounds %in% negative_control]
    concentrations <- unique(df$conc_col)
    
    # construct list of metadata
    metadata <- list(compounds = compounds,
                     concentrations = concentrations,
                     feature_cols = feature_cols,
                     negative_control = negative_control)

    class(metadata) <- "metadata"
    
    return(metadata)
}