#' @name check_functions
#' @title Checks for metaphlan derived datasets
#'
#' @description
#' Set of internal functions to check whether the dataset is a dataframe, is subset to species level,
#' has a sampleid column and the format (respectively).
#'
#' @param df dataframe to inspect.
#'
check_species_message <- function(df){
  if(!all(grepl(pattern = "s__", x = colnames(df)))){
    message("Some microorganisms are not at species level ('s__'): do you forget to subset the dataset?")
  }
}



#' Check if there are non species columns in df and returns an error
#' @param df object to inspect
#' @rdname check_functions
#'
check_species_error <- function(df){
  if(!all(grepl(pattern = "s__", x = colnames(df)))){
    stop("Some microorganisms are not at species level ('s__'). The function can give unexpected results. \n  To more information check the documentation.")
  }
}



#' Check if df is a dataframe, if not tries to convert to it.
#' @param df object to inspect
#' @rdname check_functions
#'
check_df <- function(df){
  if(is.matrix(df)){
    df <- as.data.frame(df)
  }

  if(!is.data.frame(df)){
    stop("df must be of class dataframe")
  }

  return(df)
}



#' Check sampleid column in metaphlan derived table.
#' Internal function that check if a sampleid column is present based on popular
#' choices of names for this kind of data. If the column is indeed present it is
#' removed from the dataset and returned in the list. On the contrary a NULL value
#' is reported.
#'
#' @param df dataframe to inspect.
#' @rdname check_functions
#'
check_sampleid_column <- function(df){
  if(any(grepl(pattern = "^sample|^SAMPLE|^Sample|ID$", x = colnames(df)))){
    sample_pos <- grep(pattern = "^sample|^SAMPLE|^Sample|ID$", x = colnames(df))
    sample_col <- df[, sample_pos]
    df <- df[, -sample_pos]
    message("Sampleid column identified: this will be treated separately")
  } else {
    sample_col <- NULL
  }

  return(list(df = df, sample_col = sample_col))
}



#' Check the 'format' of metaphlan tables.
#' Internal function that check whether the metaphlan derived table is in count,
#' relative abundance or percentage relative abundance. These checks are simply
#' made based on expected values limits in the different cases.
#'
#' @param df dataframe to inspect.
#' @rdname check_functions
check_table_format <- function(df){

  if(any(df > 1)){
    format <- "rel_perc"
  } else {
    format <- "rel"
  }

  if(any(df > 100)){
    format <- "count"
  }

  return(format)
}
