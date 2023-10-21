test_that("query_spec works as expected", {
  # Pretty simple tests, just to quality check the
  # "getters" to ensure they actually get, plus testing
  # some standardization, e.g. recycling
  dimensions <- c("daterangeday", "dim2", "dim3")
  company_id <- "mycompany"
  rsid <- "test-rsid"
  metrics <- c("visits", "visitors", "metric3")
  date_range <- as.Date(c("2022-01-01", "2022-01-30"))
  segment_id <- "All_Visits"

  q <- make_query_spec(
    rsid = rsid,
    company_id = company_id,
    dimensions = dimensions,
    metrics = metrics,
    date_range = date_range,
    segment_id = "All_Visits",
    limit = c(0, 5, 10),
    page = 0:2,
    search = NA,
    sort = "asc",
    include_unspecified = FALSE,
    dimensionSort = "desc"
  )

  expect_equal(
    qs_rsid(q),
    rsid
  )

  expect_equal(
    qs_company_id(q),
    company_id
  )

  expect_type(
    qs_global_filter(q),
    "list"
  )

  expect_equal(
    qs_top(q),
    c("daterangeday" = 30,
      "dim2" = 5,
      "dim3" = 10)
  )
  expect_equal(
    qs_top(q, i = 2),
    c("dim2" = 5)
  )
  expect_equal(
    qs_top(q, i = 2:3),
    c("dim2" = 5, "dim3" = 10)
  )


  expect_equal(
    qs_page(q),
    0:2
  )
  expect_equal(
    qs_page(q, i = 2),
    1
  )
  expect_equal(
    qs_page(q, i = 2:3),
    1:2
  )


  expect_equal(
    qs_search(q),
    c(NA, NA, NA)
  )
  expect_equal(
    qs_search(q, i = 2),
    NA
  )
  expect_equal(
    qs_search(q, i = 2:3),
    c(NA, NA)
  )


  expect_equal(
    qs_dimensions(q),
    dimensions
  )
  expect_equal(
    qs_dimensions(q, i = 2),
    dimensions[2]
  )
  expect_equal(
    qs_dimensions(q, i = 2:3),
    dimensions[2:3]
  )


  expect_equal(
    qs_metrics(q),
    metrics
  )


  expect_equal(
    qs_nones_behavior(q),
    "exclude-nones"
  )


  expect_equal(
    qs_dimension_sort(q),
    "desc"
  )


  expect_equal(
    qs_sort(q),
    "asc"
  )
})
