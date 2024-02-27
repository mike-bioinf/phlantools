#' @title Filter columns for minimum percentage of not-null values
#'
#' @description
#' The function allows to filter columns for which the number of not-null values
#' don't satisfy a minimum percentage threshold. In addition it's possible to set
#' a minimum abundance value (intended as a percentage relative value for microorganisms data)
#' for which the values are considered not-null. Sometimes this is desired because
#' low-abundant microorganisms have no biological meaning or effects.
#'
#'
#' @param df
#'  dataframe to filter (or matrix coerced to df).
#' @param thresh_perc
#'  Numeric; percentage threshold of not null values below the which
#'  the feature is discarded.
#' @param thresh_equal
#'  Logical (default FALSE); whether the threshold must be
#'  strictly surpassed (FALSE) or at least equalized (TRUE) by the feature to be kept.
#' @param min_abn_value
#'  Minimum abundance value (intended in percentage, e.g 5 --> means 5%) that
#'  define the presence or absence of a microorganism.
#' @param min_abn_equal
#'  Logical (Default FALSE); whether the microorganism abundance
#'  must be strictly greater (FALSE) or at least equal (TRUE) to the cutoff to be considered present.
#' @param ...
#'  Needed to pass additional argument do_check in order to block or not "format" values checking
#'  useful in case of microbial abundances data.
#'
#'
#' @return The function returns the filtered dataframe.
#' @export
#'
#' @examples
#'
#' ## In this example the relative abundance values inferior or equal to 1.5 (a.k.a 1.5%)
#' ## are considered zero. Only the features (microorganisms) for which the percentages
#' ## of not-null values are at least the 10% of the total number of values are kept.
#'
#' mpa_table <- phlantools::mpa_complete
#'
#' df_filtered <- filter_notzero_perc_cols(df = mpa_table,
#'                                         min_abn_value = 1.5,
#'                                         min_abn_equal = FALSE,
#'                                         thresh_perc = 10,
#'                                         thresh_equal = TRUE
#'                                         )
#'

filter_notzero_perc_cols <- function(df, thresh_perc, thresh_equal = FALSE, min_abn_value = 0, min_abn_equal = FALSE, ...){

  df <- check_df(df)

  if(!is.numeric(thresh_perc) || thresh_perc > 100 || thresh_perc < 0){
    stop("thresh_perc must be a numeric value from 0 to 100")
  }

  splitted <- divide_numeric_cols(df)
  df_num <- splitted$df_num
  df_other <- splitted$df_other

  log_table <- get_presence_table(df = df_num,
                                  min_abn_value = min_abn_value,
                                  min_abn_equal = min_abn_equal,
                                  ...
                                  )

  csums <- colSums(log_table)
  perc_values <- csums/nrow(df) * 100

  if(thresh_equal){
    df_filtered <- df_num[, perc_values >=  thresh_perc]
  } else {
    df_filtered <- df_num[, perc_values > thresh_perc]
  }

  if(length(df_other) > 0){
    df_filtered <- data.frame(df_other, df_filtered)
  }

  return(df_filtered)
}
