test_that("make_explicit_top works for dateless dimension lists", {
  top <- c(1, 2, 3)
  dimensions <- c("one", "two", "three")

  expect_equal(
    make_explicit_top(top, dimensions),
    c("one" = 1, "two" = 2, "three" = 3)
  )
})

test_that("make_explicit_top works for explicit date dimension lists", {
  top <- c(5, 5, 5)
  dimensions <- c("one", "daterangeday", "three")
  dimensions2 <- c("daterangeweek", "two", "three")

  expect_equal(
    make_explicit_top(top, dimensions),
    c("one" = 5, "daterangeday" = 5, "three" = 5)
  )

  expect_equal(
    make_explicit_top(top, dimensions2),
    c("daterangeweek" = 5, "two" = 5, "three" = 5)
  )
})


test_that("make_explicit_top recycles 'top' to fit dimensions", {
  top <- 5
  dimensions <- c("one", "two", "three")

  expect_equal(
    make_explicit_top(top, dimensions),
    c("one" = 5, "two" = 5, "three" = 5)
  )
})


test_that("make_explicit_top returns 0 for first-position date variables with recycled 'top' argument", {
  date_dimensions <- paste0("daterange",
                            c("minute",
                              "hour",
                              "day",
                              "week",
                              "month",
                              "quarter",
                              "year"))

  top <- 5

  # Check all date dimensions
  lapply(date_dimensions, function(datedim) {
    dimensions <- c(datedim, "two", "three")

    expect_equal(
      make_explicit_top(top, dimensions),
      setNames(c(0, 5, 5), dimensions)
    )
  })
})


test_that("make_explicit_top does not replace the first date variable's 'top' when length(dimensions) == 1", {
  date_dimensions <- paste0("daterange",
                            c("minute",
                              "hour",
                              "day",
                              "week",
                              "month",
                              "quarter",
                              "year"))

  top <- 5

  # Check all date dimensions
  lapply(date_dimensions, function(datedim) {
    dimensions <- datedim

    expect_equal(
      make_explicit_top(top, dimensions),
      setNames(5, dimensions)
    )
  })
})


test_that("make_explicit_top accepts an implied first value when length(top) == length(dimensions) - 1", {
  date <- "daterangeday"
  dimensions <- c("one", "two", "three", "four")
  top <- c(1, 2, 3, 4)

  for (i in 1:4) {
    t <- top[1:i]
    d <- c(date, dimensions[1:i])

    exp_result <- setNames(
      c(0, t),
      d
    )

    expect_equal(
      make_explicit_top(t, d),
      exp_result
    )
  }
})


test_that("make_explicit_top throws an error with incompatible top/dimension combinations", {
  # top > 1 and length(top) != length(dimensions)
  expect_error(
    make_explicit_top(1:4, c("one", "two")),
    "Invalid combination of 'top' and 'dimensions'"
  )

  # length(top) == length(dimensions) - 1 but first dimension is not a date
  expect_error(
    make_explicit_top(1:2, c("one", "two", "three")),
    "Invalid combination of 'top' and 'dimensions'"
  )
})


# These all use the same difftime function, and so can be grouped
test_that("top_daterange_number replaces zeros with correct number of units (minute, hour, day, week)", {
  datetimes <- as.POSIXct(c("2022-01-01 00:00:00", "2022-01-01 03:00:00"), format = "%F %T")
  dates <- as.Date(c("2022-01-01", "2022-01-10"))

  minute_res <- top_daterange_number(5, c("daterangeminute", "dim1", "dim2"), datetimes)
  hour_res <- top_daterange_number(5, c("daterangehour", "dim1", "dim2"), datetimes)
  day_res <- top_daterange_number(5, c("daterangeday", "dim1", "dim2"), dates)
  week_res <- top_daterange_number(5, c("daterangeweek", "dim1", "dim2"), dates)

  expect_equal(
    minute_res,
    c("daterangeminute" = 180, "dim1" = 5, "dim2" = 5)
  )
  expect_equal(
    hour_res,
    c("daterangehour" = 3, "dim1" = 5, "dim2" = 5)
  )
  expect_equal(
    day_res,
    c("daterangeday" = 10, "dim1" = 5, "dim2" = 5)
  )
  expect_equal(
    week_res,
    c("daterangeweek" = 2, "dim1" = 5, "dim2" = 5)
  )
})



test_that("top_daterange_number replaces zeros with correct number of units (month, quarter)", {
  dates <- as.Date(c("2020-05-01", "2021-04-30"))
  dims_month <- c("daterangemonth", "dim1", "dim2")
  dims_quarter <- c("daterangequarter", "dim1", "dim2")

  month_res <- top_daterange_number(c(0, 1, 2), dims_month, dates)
  quarter_res <- top_daterange_number(c(0, 1, 2), dims_quarter, dates)

  expect_equal(
    month_res,
    setNames(c(12, 1, 2), dims_month)
  )
  expect_equal(
    quarter_res,
    setNames(c(5, 1, 2), dims_quarter)
  )
})


test_that("top_daterange_number replaces zeros with correct number of units (year)", {
  dates <- as.Date(c("2019-01-01", "2022-10-31"))
  dims <- c("daterangeyear", "dim2", "dim3")

  expect_equal(
    top_daterange_number(c(0, 15, 15), dims, dates),
    setNames(
      c(4, 15, 15),
      dims
    )
  )
})



test_that("top_daterange_number also replaces 0s in the middle of the dimension list", {
  # Rotates a vector so I can vary the position of the different values
  roll <- function(x, n) {
    if (n == 0)
      return(x)
    c(tail(x, n), head(x, -n))
  }

  dates <- as.Date(c("2020-05-01", "2021-04-30"))
  dims <- c("daterangeday", "dim2", "daterangeweek", "dim4", "dim5")
  top <- c(0, 2, 0, 3, 4)

  # Base case: multiple zeros will be replaced if they satisfy the conditions
  expect_equal(
    top_daterange_number(top, dims, dates),
    setNames(c(365, 2, 53, 3, 4), dims)
  )

  # Rotate inputs to ensure position doesn't affect results
  for (i in 1:4) {
    rot_dim <- roll(dims, i)
    rot_top <- roll(top, i)

    res <- top_daterange_number(rot_top, rot_dim, dates)
    exp_res <- roll(top_daterange_number(top, dims, dates), i)

    expect_equal(
      res,
      exp_res
    )
  }
})


test_that("top_daterange_number does not replace explicit 'top' date dimension values", {
  dates <- as.Date(c("2020-05-01", "2021-04-30"))
  dims <- c("daterangeday", "dim2", "daterangeweek", "dim4", "dim5")
  top <- c(10, 2, 10, 3, 4)

  expect_equal(
    top_daterange_number(top, dims, dates),
    setNames(top, dims)
  )
})




# test_that("top_daterange_number ", {
# })


# Examples
# top_daterange_number(0, "daterangeday", as.Date(c("2021-01-01", "2021-01-10")))
# top_daterange_number(0, "daterangeday", c("2021-01-01", "2021-01-10"))