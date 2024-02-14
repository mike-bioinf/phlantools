#' Check the 'format' of species metaphlan tables
#'
#' Internal function that check whether the metaphlan derived table is in count,
#' relative abundance or percentage relative abundance. These checks are simply
#' made based on rowSums and colSums expected values.
#'
#' @param df dataframe to be checked.
#'
#' @return format, a character value equal to:
#'  1. 'rel_perc' stand for percentage relative abundances.
#'  2. 'rel' stand for non percentage relative abundances.
#'  3. 'count' stand for counts data.

check_meta_species_format <- function(df){

  if(max(rowSums(df) > 10)){
    format <- "rel_perc"
  } else {
    format <- "rel"
  }

  if(max(colSums(df)) > 100 * (nrow(df) + 1)){
    format <- "count"
  }

  return(format)
}
