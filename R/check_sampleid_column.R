#' Check sampleid column in metaphlan derived table
#'
#' Internal function that check if a sampleid column is present based on popular
#' choices of names for this kind of data. If the column is indeed present it is
#' removed from the dataset and returned in the list. On the contrary a NULL value
#' is reported.
#'
#' @param df Dataframe to inspect.

check_sampleid_column <- function(df){

  if(any(grepl(pattern = "^sample|^SAMPLE|^Sample|ID$", x = colnames(df)))){
    sample_pos <- grep(pattern = "^sample|^SAMPLE|^Sample|ID$", x = colnames(df))
    sample_col <- df[, sample_pos]
    df <- df[, -sample_pos]
    message("sampleid column identified: this will be treated separately")
  } else {
    sample_col <- NULL
  }

  return(list(df = df, sample_col = sample_col))
}
