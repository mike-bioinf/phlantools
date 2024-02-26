### helper for testing get_presence_table checking behaviours

mpa_complete <- phlantools::mpa_complete
data_lower <- mpa_complete[, 2:length(mpa_complete)]/100
data_over <- mpa_complete
data_over[1, 4] <- 160

