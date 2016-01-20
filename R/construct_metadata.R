#' Constructs metadata list to be used in functions
#' 
#' Creates a list containing all necessary metadata to subset a dataframe
#' 
#' @param df Dataframe
#' @param compound_col index or name of dataframe column
#' @param conc_col index or name of dataframe column
#' @param feature_cols Vector of indices that match numeric feature columns
#' @param negative_control string, name of negative control in compound_col
#' 
#' @return metadata list containing all metadata
#' 
#' @export


construct_metadata <- function(df, compound_col, conc_col, feature_cols, negative_control){
    
    # check input
    if(!is.data.frame(df)) stop("df needs to be a dataframe")
    if(!is.numeric(feature_cols)) stop("feature_cols needs to be a numeric vector")
    if(!is.character(negative_control)) stop("negative_control needs to be a character string")
    
    # remove negative control from compound list
    all_compounds <- unique(df[, compound_col])
    compounds <- all_compounds[! all_compounds %in% negative_control]
    concentrations <- unique(df[, conc_col])

    # warning message if any compound or concentration are NA
    if (sum(is.na(all_compounds)) > 0){
        warning("compound list contains NAs")
    }
    if (sum(is.na(concentrations)) > 0){
        warning("concentration list contains NAs")
    }
    
    # construct list of metadata
    metadata <- list(compounds = compounds,
                     concentrations = concentrations,
                     feature_cols = feature_cols,
                     negative_control = negative_control,
                     compound_col = compound_col)

    class(metadata) <- "metadata"
    
    return(metadata)
}