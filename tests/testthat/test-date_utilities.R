test_that("make_timeframe works as expected", {
  dates_str <- c("2021-01-01", "2021-01-10")
  dates_date <- as.Date(dates_str)
  dates_datetime <- as.POSIXct(c("2021-01-01 00:00:00", "2021-01-10 23:59:59"), format = "%F %T")

  str_result <- make_timeframe(dates_str)
  date_result <- make_timeframe(dates_date)
  datetime_result <- make_timeframe(dates_datetime)

  expect_type(str_result, "character")
  expect_type(date_result, "character")
  expect_type(datetime_result, "character")
  expect_length(str_result, 1)
  expect_length(date_result, 1)
  expect_length(datetime_result, 1)

  expect_equal(
    str_result,
    "2021-01-01T00:00:00/2021-01-10T23:59:59"
  )
  expect_equal(str_result, date_result)
  expect_equal(date_result, datetime_result)
})


test_that("make_timeframe throws a warning for numeric inputs", {
  dates_num <- c(18997L, 18100L)

  expect_warning(
    make_timeframe(dates_num),
    "Numeric values for date range will be converted to dates"
  )
})


test_that("make_timeframe throws an error if length != 2", {
  expect_error(
    make_timeframe("2021-01-01"),
    "is not TRUE"
  )
})

