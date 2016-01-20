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
    
    if (class(metadata) != "metadata"){
        stop("metadata has to be of the class 'metadata'")
    }
    if (!is.data.frame(df)) stop("df has to be a dataframe")
    if (!is.list(metadata)) stop("metadata has to be a list")

    # remove negative control data
    df_no_n_cntl <- df[df[, metadata$compound_col] != metadata$negative_control, ]
    
    # remove compounds and concentrations that are NAs
    if (sum(is.na(metadata$compounds)) > 0){
        warning("Removing NA compound data")
    }
    if (sum(is.na(metadata$concentrations)) > 0){
        warning("Removing NA concentration data")
    }

    # remove rows where compound == NA
    df_no_na_ <- df[ !is.na(df[, metadata$concentration_col]), ]
    # remove rows where concentration == NA
    df_no_na <- df_no_na_[ !is.na(df_no_na_[, metadata$compound_col]), ]

    # remove compound which contained NA values
    # get compounds with no NAs
    has_NA <- df[ is.na(df[, metadata$concentration_col]), ]
    NA_cmpd <- unique(has_NA[, metadata$compound_col])
    compounds_full <- metadata$compounds[!metadata$compounds %in% NA_cmpd]
    df_full <- df_no_na[ df_no_na[, metadata$compound_col] %in% compounds_full, ]
    
    # remove NA from concentrations if present
    concentrations_full <- metadata$concentrations[!is.na(metadata$concentrations)]
    
    # list of compounds
    split_by_compound <- split(df_no_na, compounds_full)
    # each element 'compound' contains list of dataframes for each concentration
    split_all <- sapply(split_by_compound, split, f = concentrations_full)
    
    # subset featuredata
    split_featuredata <- sapply(split_all, '[', metadata$feature_cols)
    
    return(split_featuredata)
}