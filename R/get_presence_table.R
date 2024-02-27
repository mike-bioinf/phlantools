#' Internal function to obtain a presence/absence table from an abundance table
#'
#' The function checks the format of the abundances table simply on expectation
#' about its values and takes action accordingly.
#'
#' @param df Dataframe from which obtain the presence/absence table.
#' @param min_abn_value Numeric value. Sometimes only abundance values greater than a minimum value
#'  have to be considered as presence. This value is intended as a percentage value for microbial abundances
#'  (e.g 5 means 5% for relative abundances).
#' @param min_abn_equal logical (default FALSE); if a value must be equal or greater (TRUE) or
#'  strictly greater (FALSE) than the min_abn_value to be considered a presence.
#' @param do_check logical (default TRUE); whether to check for relative abundances values useful
#'  in case of microbial data.
#'
#' @return The function returns the presence/absence table as LOGICAL table (i.e. presence is
#' encoded as 1 and absence as 0).


get_presence_table <- function(df, min_abn_value = 0, min_abn_equal = FALSE, do_check = TRUE){

  if(!is.numeric(min_abn_value)){
    stop("min_abn_value must be a numeric value")
  }

  if(!is.logical(min_abn_equal)){
    stop("min_abn_equal is not set as logical value")
  }

  if(min_abn_value == 0 && min_abn_equal){
    message("min_abn_value is set to 0 and min_abn_equal to TRUE: every possible value is considered a presence")
  }


  if(do_check){
    if(all(df < 1.1)){
      warning("The df's values are interpreted as non-percentage relative abundances: the dataset is multiplied by a 100 factor.
               To block the check and subsequent changes pass the argument do_check = FALSE")
      df_checked <- df * 100
    } else if(any(df > 100.1)){
      stop("The df's values are interpreted as counts (there is a value greater than 100): convert to relative abundances to go on.
           To block the check pass the argument do_check = FALSE")
    } else {
      df_checked <- df
    }
  } else {
    df_checked <- df
  }

  if(min_abn_equal){
    log_table <- as.data.frame(df_checked >= min_abn_value)
  } else {
    log_table <- as.data.frame(df_checked > min_abn_value)
  }

  return(log_table)
}
