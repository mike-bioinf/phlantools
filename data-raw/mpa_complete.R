## General metaphlan table with all taxonomical level and sampleid column

mpa_complete <- read.csv(file = "data-raw/mpa_complete.txt", header = T, sep = "\t")
mpa_complete <- data[1:20, ]

usethis::use_data(mpa_complete, overwrite = TRUE)
