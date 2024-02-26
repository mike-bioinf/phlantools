### helper for the construction of data to test filt_notzero_perc_cols

v1 <- c(0, 0, 0, 0, 0, 0, 0, 2, 3, 3)
v2 <- c(0, 0, 0, 0, 0, 4, 4, 5, 5, 5)
v3 <- c(4, 4, 5, 5, 5, 5, 6, 6, 6, 6)
v4 <- c(1, 2, 12, 12, 12, 15, 15, 15, 15, 15)

df2filt <- cbind(v1, v2, v3, v4)

## f stands for filtered;
## the first number refer to the threshold of not null values;
## the second one to the cutoff for identifying a presence or absence;
## the eq stands for equal (neq for not equal) and refers to the equal option for both thresh (so eq = equal = TRUE)
##   expect for the min_abn_equal when the value is zero (in this case all values are considered presence).

f_30_1_eq <- data.frame(v1, v2, v3, v4)
f_30_0_neq <- data.frame(v2, v3, v4)
f_70_5_eq <- data.frame(v3, v4)
f_70_5_neq <- v4
