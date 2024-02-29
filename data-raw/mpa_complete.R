## General metaphlan table with all taxonomical level and sampleid column

mpa_complete <- read.csv(file = "data-raw/mpa_complete.txt", header = T, sep = "\t")
mpa_complete <- mpa_complete[1:20, 1:100]
usethis::use_data(mpa_complete, overwrite = TRUE)
