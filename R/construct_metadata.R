#' Constructs metadata list to be used in functions
#' 
#' Creates a list containing all necessary metadata to subset a dataframe.
#' Used within other functions as a useful container and stops repeated
#' input of dataframe information.
#' 
#' @param df Dataframe
#' @param compound_col index or name of dataframe column for compound names
#' @param conc_col index or name of dataframe column for concentration data
#' @param feature_cols Vector of indices that match numeric feature columns.
#'  a feature column is one that contains numerical information of a cellular
#'  measurement such as area, or cell_count. Metadata such as image_number or
#'  object_number_ID is not feature data.
#' @param negative_control string, name of negative control in compound_col.
#' e.g \code{'DMSO'}
#' 
#' @return metadata List containing all metadata.
#' 
#' @export
#'
#' @examples
#' # construct example dataframe
#' concentration <- rep(c(100, 10, 1, 0.1), 10)
#' compounds <- sort(rep(letters[1:10], 4))
#' cell_area <- rnorm(40)
#' no_speckles <- rnorm(40, 100, 100)
#' df <- data.frame(compounds, concentration, cell_area, speckles)
#'
#' construct_metadata(
#'     df,
#'     compound_col = "compounds",
#'     conc_col = "concentration",
#'     feature_cols = 3:4,
#'     negative_control = "a")


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
                     compound_col = compound_col,
                     concentration_col = conc_col)

    class(metadata) <- "metadata"
    
    return(metadata)
}