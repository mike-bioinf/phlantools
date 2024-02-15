#' Internal function to obtain a presence/absence table from an abundance table
#'
#' The function checks the format of the abundances table based simply on expectation
#' about its values and takes action accordingly.
#'
#' @param df Dataframe from which obtain the presence/absence table.
#' @param min_abn_value numeric value. Sometimes only abundance values greater than a minimum value
#'  have to be considered as presence. This value MUST be specified as percentage value (e.g 5 --> that means 5%).
#' @param min_abn_equal logical (default FALSE); if a value must be equal or greater (TRUE) or
#'  strictly greater (FALSE) than the min_abn_value to be considered a presence.
#'
#' @return The function returns the presence/absence table.


get_presence_table <- function(df, min_abn_value = 0, min_abn_equal = FALSE){

  if(!is.numeric(min_abn_value)){
    stop("min_abn_value must be a numeric value")
  }

  if(!is.logical(min_abn_equal)){
    stop("min_abn_equal is not set as logical value")
  }

  if(min_abn_value == 0 & min_abn_equal){
    message("min_abn_value is set to 0 and min_abn_equal to true: every possible value is considered a presence")
  }

  if(all(df < 1.1)){
    message("The df's values are interpreted as non-percentage relative abundances: the dataset is multiplied by a 100 factor")
    df_res <- df * 100
  } else if(any(df > 100)){
    stop("The df's values are interpreted as counts (there is a value greater than 100):
         check and convert to relative abundances to go on")
  } else {
    df_res <- df
  }

  for(i in 1:nrow(df_res)){
    for(j in seq_along(df_res)){
     if(min_abn_equal){
       if(df_res[i, j] >= min_abn_value){
         df_res[i, j] <- 1
       } else {
         df_res[i, j] <- 0
       }
     } else {
       if(df_res[i, j] > min_abn_value){
          df_res[i, j] <- 1
       } else {
          df_res[i, j] <- 0
       }
     }
    }
  }

  return(df_res)
}
