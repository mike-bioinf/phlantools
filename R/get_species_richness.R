#' Get species richness index.
#'
#' @description
#' The function allow to compute the species richness index.
#'
#'
#' @param df
#'  dataframe of abundances data.
#' @param margin
#'  numeric; it can be set only equal to 1, case in which the microbial features are
#'  on the rows, and 2 (default value), case in which the microbial features are on the columns.
#' @param min_abn_value
#'  Numeric value. Sometimes only abundance values greater than a minimum value
#'  have to be considered as presence. This value is intended as a percentage value for microbial abundances
#'  (e.g 5 means 5% for relative abundances).
#' @param min_abn_equal
#'  logical (default FALSE); if a value must be equal or greater (TRUE) or
#'  strictly greater (FALSE) than the min_abn_value to be considered a presence.
#' @param ...
#'  Needed to pass the additional argument do_check in order to block or not "format" values checking
#'  useful in case of microbial abundances data.
#'
#' @return
#'  The function return a dataframe with the richness score plus the non-numeric columns
#'  in case of margin = 2, or the names of columns in case of margin = 1.
#'
#' @export
#' @examples
#'
#'
#' mpa_complete <- phlantools::mpa_complete
#' richness_df <- get_species_richness(df = mpa_complete, margin = 2)
#'
#' ## The function will return a message highlighting that also taxa not identified
#' ## to species level are present and therefore used for the computation of the index.
#' ## The function doesn't automatically select the species level taxa.

get_species_richness <- function(df, margin = 2, min_abn_value = 0, min_abn_equal = FALSE, ...){

  if(!is.numeric(margin) && any(margin %in% c(1, 2))){
    stop("margin must be a numeric value equal to 1 or 2")
  }

  df <- check_df(df)

  if(margin == 1){
    num_check <- purrr::map_lgl(df, is.numeric)
    if(any(!num_check)){
      message("unexpected non numeric columns identified: they are discarded from the computations")
      df <- as.data.frame(t(purrr::keep(.x = df, .p = is.numeric)))
    } else {
      df <- as.data.frame(t(df))
    }
  }

  check_species_message(df)
  splitted <- divide_numeric_cols(df)
  df_num <- splitted$df_num
  df_other <- splitted$df_other

  log_table <- get_presence_table(df = df_num,
                                  min_abn_value = min_abn_value,
                                  min_abn_equal = min_abn_equal,
                                  ...
                                  )

  richness_table <- rowSums(log_table)

  if(length(df_other) > 0){
    richness_table <- data.frame(df_other, richness_score = richness_table)
  }

  if(margin == 1){
    richness_table <- data.frame(col_names = rownames(df), richness_score = richness_table)
  }

  return(richness_table)
}

