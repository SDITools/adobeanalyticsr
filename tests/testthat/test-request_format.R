# Global filter ----
test_that("global_filter_elem has expected output", {
  filt_1 <- global_filter_elem(type = "daterange",
                     dateRange = "really-long-daterange-string")

  filt_2 <- global_filter_elem(segmentId = "segid",
                     type = "segment")

  filt_1_exp <- list(type = "daterange", dateRange = "really-long-daterange-string")
  filt_2_exp <- list(type = "segment", segmentId = "segid")

  expect_identical(filt_1, filt_1_exp)
  expect_identical(filt_2, filt_2_exp)
})


test_that("global_filter has expected output", {
  # Single segment ID
  filt_1 <- global_filter(segmentId = c("segid"), dateRange = "really-long-daterange-string")
  filt_1_exp_a <- list(type = "daterange", dateRange = "really-long-daterange-string")
  filt_1_exp_b <- list(type = "segment", segmentId = "segid")

  # Multiple segment IDs
  filt_2 <- global_filter(segmentId = c("segid1", "segid2", "segid3"), dateRange = "really-long-daterange-string")
  filt_2_exp_a <- list(list(type = "daterange", dateRange = "really-long-daterange-string"))
  filt_2_exp_b <- list(
    list(type = "segment", segmentId = "segid1"),
    list(type = "segment", segmentId = "segid2"),
    list(type = "segment", segmentId = "segid3")
  )


  expect_identical(filt_1, list(filt_1_exp_a, filt_1_exp_b))
  expect_identical(filt_2, c(filt_2_exp_a, filt_2_exp_b))
})


test_that("global_filter handles NULLs", {
  expect_identical(
    global_filter(dateRange = "really-long-daterange-string"),
    list(list(type = "daterange", dateRange = "really-long-daterange-string"))
  )

  expect_identical(
    global_filter(segmentId = "some-segment-id"),
    list(list(type = "segment", segmentId = "some-segment-id"))
  )
})


test_that("global_filter throws an error with more than one daterange filter", {
  expect_error(
    global_filter(segmentId = c("seg1", "seg2"), dateRange = c("date1/date2", "date3/date4")),
    "More than one date range specified"
  )
})


# Metric elements ----
test_that("metric_elem works as expected", {
  elem1 <- metric_elem(id = 'theID', columnId = "colID", filters = "filt1", sort = "asc")
  elem2 <- metric_elem(id = 'theID', columnId = "colID", filters = "filt1")
  elem3 <- metric_elem(id = 'theID', columnId = "colID")

  res1 <- list(id = 'theID', columnId = "colID", filters = I("filt1"), sort = "asc")
  res2 <- list(id = 'theID', columnId = "colID", filters = I("filt1"))
  res3 <- list(id = 'theID', columnId = "colID")

  expect_identical(elem1, res1)
  expect_identical(elem2, res2)
  expect_identical(elem3, res3)
})

test_that("metric_elem throws the expected errors", {
  expect_error(
    metric_elem(id = 10, columnId = "colId"),
    "id is not a character vector"
  )

  expect_error(
    metric_elem(id = "id", columnId = 10),
    "columnId is not a character vector"
  )
})

test_that("metric_elem converts sort NAs to NULL", {
  expect_identical(
    metric_elem("id", "colid", sort = NA),
    list(id = "id", columnId = "colid")
  )
})



test_that("metric_elems works as expected", {
  elems <- metric_elems(id = c("met1", "met2"),
               columnId = as.character(1:2),
               filters = list("one", c("one", "two")),
               sort = c("asc", NA))

  exp <- list(
    list(
      id = "met1",
      columnId = "1",
      filters = I(c("one")),
      sort = "asc"
    ),
    list(
      id = "met2",
      columnId = "2",
      filters = I(c("one", "two"))
    )
  )

  expect_identical(elems, exp)
})


# Metric filters ----
test_that("metric_filters works as expected", {
  metfilt_1 <- metric_filters(
    type = c("segment", "breakdown", "breakdown", "dateRange"),
    segmentId = c("s1234567890_09583204824324"),
    dimension = c("evar45", "prop11"),
    itemId = c("1234", "5678"),
    dateRange = "today/tomorrow"
  )


  metfilt_2 <- metric_filters(
    type = c("segment", "breakdown"),
    segmentId = c("s1234567890_09583204824324"),
    dimension = "evar45",
    itemId = "1234"
  )

  exp_1 <- data.frame(
    id = c("dateRange", "evar45", "prop11", "s1234567890_09583204824324"),
    type = c("dateRange", "breakdown", "breakdown", "segment"),
    dateRange = c("today/tomorrow", NA, NA, NA),
    dimension = c(NA, "evar45", "prop11", NA),
    itemId = c(NA, "1234", "5678", NA),
    segmentId = c(NA, NA, NA, "s1234567890_09583204824324")
  )

  exp_2 <- data.frame(
    id = c("evar45", "s1234567890_09583204824324"),
    type = c("breakdown", "segment"),
    dimension = c("evar45", NA),
    itemId = c("1234", NA),
    segmentId = c(NA, "s1234567890_09583204824324")
  )

  expect_identical(metfilt_1, exp_1)
  expect_identical(metfilt_2, exp_2)
})
