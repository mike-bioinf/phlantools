#' Split dataframe in numeric and non numeric columns
#'
#' Internal function used for splitting a dataframe in a list of 2 dataframes:
#' one made of numeric columns and the other made of other types columns.
#'
#' @param df Dataframe to be splitted.
#' @return List of thow dataframes: df_num and df_other. If the orginal df is comprised of
#' only numeric columns, then df_other is a dataframe with 0 rows and columns.


divide_numeric_cols <- function(df){

  other_cols <- list()
  num_cols <- list()

  for(n in colnames(df)){
    if(is.numeric(df[[n]])){
      num_cols[[n]] <- df[[n]]
    } else {
      other_cols[[n]] <- df[[n]]
    }
  }

  df_num <- as.data.frame(num_cols)
  df_other <- as.data.frame(other_cols)

  if(length(df_other) > 0){
    message("non-numeric columns identified in the dataframe: they are treated separately")
  }

  return(list(df_num = df_num, df_other = df_other))
}
