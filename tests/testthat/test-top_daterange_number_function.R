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

  expect_equal(
    make_explicit_top(top, dimensions),
    c("one" = 5, "daterangeday" = 5, "three" = 5)
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





test_that("top_daterange_number replaces zeros with correct number of units", {
  # Days

})


# test_that("top_daterange_number ", {
# })


# Examples
# top_daterange_number(0, "daterangeday", as.Date(c("2021-01-01", "2021-01-10")))
# top_daterange_number(0, "daterangeday", c("2021-01-01", "2021-01-10"))