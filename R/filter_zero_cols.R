#' @title Filter numeric columns containing only zero values
#'
#' @description
#' The function filter the numeric columns of only zero values. The non-numeric
#' columns are not touched and collected at beginning of the dataset.
#'
#' @param df Dataframe to filter.
#'
#' @return The function returns the filtered dataframe.
#' @export


filter_zero_cols <- function(df){

  if(!is.data.frame(df)){
    stop("df must be of class dataframe")
  }

  splitted <- divide_numeric_cols(df = df)
  df_num <- splitted$df_num
  df_filtered <- df_num[, colSums(df_num) != 0]

  if(length(splitted$df_other) > 0){
    df_filtered <- data.frame(splitted$df_other, df_filtered)
  }

  return(df_filtered)
}
