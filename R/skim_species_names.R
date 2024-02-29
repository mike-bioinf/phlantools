#' Simplify species taxonomy names.
#'
#' @description
#' Species names can be full of alphanumeric codes. This function simplify their names
#' by retrieving the first non alphanumerical taxonomic level and cutting the above portion.
#' If the resulting names are still too long, the skip_middle option allow to filter out the
#' "central" portion of taxonomy, displaying only the first unambiguous level along
#' with species levels ("s__" and "t__" information).
#' In addition, the function cleans the names of columns without alphanumeric capital codes
#' (and therefore full specified taxonomically) to the species level.
#'
#'
#' @param df
#'  dataframe of microbial species like a metaphlan table.
#' @param skip_middle
#'  Logical (Default FALSE); if the middle portion of long 'unspecific' taxonomic names
#'  must be skipped (only the first unambiguous taxonomic level along with s and t information
#'  is kept).
#' @param split_pattern
#'  strsplit regex (default "\\."). The function internally uses strsplit to divide
#'  the columns names of the dataframe based on the specified pattern, and to select
#'  the first unambiguous part. Option useful to have more control and options to deal with
#'  different "taxonomic format".
#' @param do_check_species
#'  Logical (default TRUE). An option to allow or block the taxonomic level checking
#'  of features (the pattern "s__" is sought). In the presence of features not defined as species,
#'  or without the scheme encoded by 'letter + __', the function may produce unexpected results.
#'
#' @export
#' @return The function returns the dataframe with the adjusted names.


skim_species_names <- function(df, skip_middle = F, split_pattern = "\\.", do_check_species = TRUE){

  df <- check_df(df)
  splitted <- divide_numeric_cols(df)
  df_num <- splitted$df_num
  df_other <- splitted$df_other

  if(do_check_species){
    check_species_error(df_num)
  }

  names <- colnames(df_num)
  log <- grepl(pattern = "[[:upper:]]+[[:digit:]]", x = names)
  nam <- grep(pattern = "[[:upper:]]+[[:digit:]]", x = names, value = T)
  spl_nam <- strsplit(x = nam, split = split_pattern)

  sel_nam <- lapply(spl_nam, function(v){
    norm_ind <- grep("[[:upper:]]+[[:digit:]]", v, invert = T)
    sel_norm <- max(norm_ind)

    if(skip_middle){
      if(sel_norm == (length(v) - 1)){
        res_nam <- paste(v[sel_norm], v[length(v)], sep = "__")
      } else {
        res_nam <- paste(v[sel_norm], v[length(v) - 1], v[length(v)], sep = "__")
      }
    } else{
      res_nam <- paste(v[sel_norm], paste(v[grep("[[:upper:]]+[[:digit:]]", v)], collapse = "__"), sep = "__")
    }
  })

  colnames(df_num)[log] <- unlist(sel_nam, use.names = F)

  nam_full <- colnames(df_num)[!log]
  nam_full_mod <- gsub(pattern = ".*s__", replacement = "", x = nam_full)
  nam_full_mod <- paste0("s__", nam_full_mod)
  colnames(df_num)[!log] <- nam_full_mod

  if(length(df_other) > 0){
    df_num <- data.frame(df_other, df_num)
  }

  return(df_num)
}
