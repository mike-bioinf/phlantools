% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/skim_species_names.R
\name{skim_species_names}
\alias{skim_species_names}
\title{Simplify species taxonomy names.}
\usage{
skim_species_names(
  df,
  skip_middle = F,
  split_pattern = "\\\\.",
  do_check_species = TRUE
)
}
\arguments{
\item{df}{dataframe of microbial species like a metaphlan table.}

\item{skip_middle}{Logical (Default FALSE); if the middle portion of long 'unspecific' taxonomic names
must be skipped (only the first unambiguous taxonomic level along with s and t information
is kept).}

\item{split_pattern}{strsplit regex (default "\\."). The function internally uses strsplit to divide
the columns names of the dataframe based on the specified pattern, and to select
the first unambiguous part. Option useful to have more control and options to deal with
different "taxonomic format".}

\item{do_check_species}{Logical (default TRUE). An option to allow or block the taxonomic level checking
of features (the pattern "s__" is sought). In the presence of features not defined as species,
or without the scheme encoded by 'letter + __', the function may produce unexpected results.}
}
\value{
The function returns the dataframe with the adjusted names.
}
\description{
Species names can be full of alphanumeric codes. This function simplify their names
by retrieving the first non alphanumerical taxonomic level and cutting the above portion.
If the resulting names are still too long, the skip_middle option allow to filter out the
"central" portion of taxonomy, displaying only the first unambiguous level along
with species levels ("s__" and "t__" information).
In addition, the function cleans the names of columns without alphanumeric capital codes
(and therefore full specified taxonomically) to the species level.
}
