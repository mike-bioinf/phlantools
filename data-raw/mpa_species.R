### Species level metaphlan table with sampleid column

mpa_complete <- read.csv(file = "data-raw/mpa_complete.txt", header = T, sep = "\t")
mpa_species <- select(mpa_complete, contains("s__"))[1:20, 1:199]
mpa_species <- data.frame(sampleID = mpa_complete$sampleID[1:20], mpa_species)
usethis::use_data(mpa_species, overwrite = T)
