#' @title Filter numeric columns containing only zero values
#'
#' @description
#' The function filter the numeric columns that consist of only zero values. The non-numeric
#' columns are not touched and collected at the beginning of the dataset.
#'
#' @param df dataframe to filter.
#'
#' @return The function returns the filtered dataframe.
#' @export

filter_zero_cols <- function(df){

  df <- check_df(df)
  splitted <- divide_numeric_cols(df = df)
  df_num <- splitted$df_num
  df_filtered <- df_num[, colSums(df_num) != 0]

  if(length(splitted$df_other) > 0){
    df_filtered <- data.frame(splitted$df_other, df_filtered)
  }

  return(df_filtered)
}
