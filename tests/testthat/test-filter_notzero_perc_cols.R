## testing filter_notzero_perc_cols and therefore its base function get_presence table

test_that("filter_notzero_perc_cols works",{

  dff_30_1_eq <- filter_notzero_perc_cols(df = df2filt,
                                          thresh_perc = 30,
                                          thresh_equal = TRUE,
                                          min_abn_value = 1,
                                          min_abn_equal = TRUE
                                          )

  expect_identical(object = dff_30_1_eq, expected = f_30_1_eq)

  dff_30_0_neq <- filter_notzero_perc_cols(df2filt, 30, FALSE, 0, FALSE)
  expect_identical(object = dff_30_0_neq, expected = f_30_0_neq)

  dff_70_5_eq <- filter_notzero_perc_cols(df2filt, 70, TRUE, 5, TRUE)
  expect_identical(dff_70_5_eq, f_70_5_eq)

  dff_70_5_neq <- filter_notzero_perc_cols(df2filt, 70, FALSE, 5, FALSE)
  expect_identical(dff_70_5_neq, f_70_5_neq)
})
