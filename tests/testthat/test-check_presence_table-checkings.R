### test get_presence_table checking behaviour

test_that(" testing the checking behaviour of get_presence_table",{
  expect_warning(
    get_presence_table(df = data_lower),
    "The df's values are interpreted as non-percentage relative abundances"
    )

  expect_error(
    get_presence_table(df = data_over),
    "The df's values are interpreted as counts"
  )

  expect_no_warning(get_presence_table(df = data_lower, do_check = FALSE))

  expect_no_error(get_presence_table(df = data_over, do_check = FALSE))
})

