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