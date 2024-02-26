#' Get the percentage presence of all numeric columns.
#'
#' @description
#' The function allows to compute the percentage presence of all numeric features of a dataframe,
#' in conjunction with a minimum cutoff to establish their state of presence/absence.
#'
#' @param df dataframe
#' @param min_abn_value Numeric value. Sometimes only abundance values greater than a minimum cutoff
#'  have to be considered as presence. This value is intended as a percentage value for microbial abundances data
#'  (e.g 5 means 5% in relative terms).
#' @param min_abn_equal Logical value (default FALSE); if a value must be equal or greater (TRUE) or
#'  strictly greater (FALSE) than the min_abn_value to be considered as a presence.
#' @param ... Needed to pass the additional argument 'do_check' that allows to block or not "format values checking",
#'  useful in case of microbial abundances data.
#'
#' @return Named vector of percentage presence values.
#' @export
#'
#' @examples
#' ## In this example the relative abundance values inferior or equal to 1.5 (a.k.a 1.5%)
#' ## are considered zero.
#'
#' mpa_table <- phlantools::mpa_complete
#' prevalence_vector <- get_presence_perc(df = mpa_complete,
#'                                        min_abn_value = 1.5,
#'                                        min_abn_equal = FALSE)

get_presence_perc <- function(df, min_abn_value = 0, min_abn_equal = FALSE, ...){

  df <- check_df(df)
  splitted <- divide_numeric_cols(df)
  df_num <- splitted$df_num
  log_table <- get_presence_table(df_num, ...)
  prevalence_vector <- colSums(log_table)/nrow(df) * 100
}

