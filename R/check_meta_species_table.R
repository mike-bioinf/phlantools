#' Check metaphlan derived species datasets
#'
#' The internal function checks whether the dataset is subset to species level through the
#' 's__' pattern searched in all column names .
#'
#' @param df Species dataset derived from metaphlan profile
#'

check_meta_species_table <- function(df){

  if(!all(grepl(pattern = "s__", x = colnames(df)))){
    message("some columns are not referring to species ('s__'): do you forget to subset the dataset?")
  }

}
