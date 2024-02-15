#' @title Filter columns for minimum percentage of not-null values
#'
#' @description
#'
#' The function allows to filter columns for which the number of not-null values
#' don't satisfy a minimum percentage threshold. In addition it's possible to set
#' a minimum abn (interpreted as a percentage value) for which the values are considered
#' not-null. Sometimes this is desired because low-abundant microorganisms have no
#' biological meaning or effects.
#'
#'
#'
#' @param df Dataframe to filter.
#' @param thresh_perc numeric; percentage threshold of not null values below the which
#'  the microorganism is discarded
#' @param thresh_equal logical (default FALSE); whether the threshold must be
#'  strictly surpassed (FALSE) or at least equalized (TRUE) by the microorganism to be kept.
#' @param min_abn_value Minimum abundance value (in percentage, e.g 5 --> means 5%) that
#' define the presence or absence of a microorganism.
#' @param min_abn_equal logical (Default FALSE); whether the microorganism abundance
#'  must be strictly greater (TRUE) or at least equal (FALSE) to the cutoff to be considered present.
#'
#'
#' @return The function returns the filtered dataframe.
#' @export
#'
#'
#' @examples
#'
#' ## In this example the relative abundances values inferior and equal to 1.5 (a.k.a 1.5%)
#' ## are considered zero. Only the features (microorganisms) for which the percentages
#' ## of not-null values are at least the 10% of the total number of values are kept.
#'
#' mpa_table <- phlantools::mpa_complete
#' df_filtered <- filter_notzero_perc_cols(df = mpa_table,
#'                                          min_abn_value = 1.5,
#'                                          min_abn_equal = FALSE,
#'                                          thresh_perc = 10,
#'                                          thresh_equal = TRUE
#'                                         )
#'

filter_notzero_perc_cols <- function(df, thresh_perc, thresh_equal = FALSE, min_abn_value = 0, min_abn_equal = FALSE){

  if(!is.data.frame(df)){
    stop("df must be of class dataframe")
  }

  splitted <- divide_numeric_cols(df = df)
  df_num <- splitted$df_num
  df_other <- splitted$df_other

  log_table <- get_presence_table(df = df_num,
                                  min_abn_value = min_abn_value,
                                  min_abn_equal = min_abn_equal
                                  )

  csums <- colSums(log_table)
  perc_values <- csums/nrow(df) * 100

  if(thresh_equal){
    df_res <- df_num[, perc_values >=  thresh_perc]
  } else {
    df_res <- df_num[, perc_values > thresh_perc]
  }

  if(length(df_other) > 0){
    df_res <- data.frame(df_other, df_res)
  }

  return(df_res)
}
