#' Get the percentage presence of all numeric columns for levels of a factor.
#'
#' @description
#' The function allows to compute the percentage presences of all numeric columns,
#' in conjunction with a minimum cutoff of presence/absence, all in respect to the
#' levels of a factor variable.
#'
#' @param df dataframe with numeric columns and factor feature of interest.
#' @param cat_var
#'  Character value reporting the name of the column variable on which
#'  levels the percentages of numeric columns are computed. Not-character columns are automatically
#'  coerced to factors.
#' @param min_abn_value
#'  Numeric value. Sometimes only abundance values greater than a minimum value
#'  have to be considered as presence. This value is intended as a percentage value for microbial abundances
#'  (e.g 5 means 5% in relative abundances).
#' @param min_abn_equal
#'  Logical (default FALSE); if a value must be equal or greater (TRUE) or
#'  strictly greater (FALSE) than the min_abn_value to be considered as a presence.
#' @param perc_on_all
#'  Logical (default FALSE); if the percentages must be computed on the total
#'  number of rows/observations of the dataframe (TRUE) or on the number of observations of each group (FALSE).
#' @param ...
#'  Needed to pass the additional argument 'do_check' that allows to block or not "format values checking",
#'  useful in case of microbial abundances data.
#'
#' @return A dataframe with the numeric features in rows and the grouping levels on columns.
#'  The cells report the percentage values.
#'
#' @export
# ## see the documentation of get_presence_perc.

get_presence_perc_cat <- function(df, cat_var, min_abn_value = 0, min_abn_equal = FALSE, perc_on_all = FALSE, ...){

  df <- check_df(df)
  stopifnot("cat_var must pass the name of the column as character value" = is.character(cat_var))

  if(!any(colnames(df) %in% cat_var)){
    stop(paste0("There is no column ", cat_var, " in df"))
  }

  if(!is.character(df[[cat_var]]) && !is.factor(df[[cat_var]])){
    message("cat_var is neither a factor or character column: do you expect this?")
  }

  splitted_cat <- split(x = df, f = df[[cat_var]])

  if(perc_on_all){
    num_rows <- nrow(df)
  }

  df_perc <- as.data.frame(lapply(splitted_cat, function(d){
    if(!perc_on_all){
      num_rows <- nrow(d)
    }

    d <- purrr::keep(d, is.numeric)
    log_table <- get_presence_table(d, min_abn_value = min_abn_value, min_abn_equal = min_abn_equal, ...)
    perc_values <- colSums(log_table)/num_rows * 100
  }))
}
