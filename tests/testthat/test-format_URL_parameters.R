# The goal of the unit tests is to make sure basic functionality is stable
# despite my constant tinkering

# Testing functions that do not require internet

test_that("Format URL converts a list to a single string", {
  params <- list(
    param1 = "first",
    param2 = "second",
    param3 = "third"
  )

  params_vec <- list(
    param1 = c("one", "two", "three"),
    param2 = 5:10
  )

  param_url <- format_URL_parameters(params)
  param_vec_url <- format_URL_parameters(params_vec)

  expect_type(param_url, "character")
  expect_length(param_url, 1)
  expect_type(param_vec_url, "character")
  expect_length(param_vec_url, 1)
})


test_that("Format URL params encodes special characters into percent encoding", {
  params <- list(
    param1 = c("_Special (Symbols).", "Check me 2!")
  )

  param_url <- format_URL_parameters(params)

  expect_equal(
    param_url,
    "param1=_Special%20%28Symbols%29.%2CCheck%20me%202%21"
  )
})


test_that("Format URL parameters URL encodes list names", {
  params <- list(
    "_Special (Symbols)." = "symbolic"
  )

  param_url <- format_URL_parameters(params)

  expect_equal(
    param_url,
    "_Special%20%28Symbols%29.=symbolic"
  )
})


test_that("Format URL removes empty elements from the list", {
  params <- list(
    param1 = "first",
    param2 = NULL
  )

  params2 <- list(
    x = NULL,
    y = NULL
  )

  param_url <- format_URL_parameters(params)
  param_url2 <- format_URL_parameters(params2)

  expect_equal(
    param_url,
    "param1=first"
  )

  expect_equal(
    param_url2,
    ""
  )
})


test_that("Format URL parameters returns an empty string when given an empty list", {
  params <- list()
  expect_equal(
    format_URL_parameters(params),
    ""
  )
})


test_that("Format URL handles named character vectors", {
  params <- list(
    param1 = c("one" = "ONE", "two" = "TWO", "three" = "THREE")
  )

  param_url <- format_URL_parameters(params)

  expect_type(param_url, "character")
  expect_length(param_url, 1)
  expect_equal(
    param_url,
    "param1=ONE%2CTWO%2CTHREE"
  )
})


test_that("Format URL parameters converts NAs to strings", {
  params <- list(
    param1 = c("one", NA, "three")
  )

  param_url <- format_URL_parameters(params)

  expect_equal(
    param_url,
    "param1=one%2CNA%2Cthree"
  )
})


test_that("Format URL parameters leaves AsIs alone", {
  params <- list(param1 = I(c("_Special (Symbols).", "Check me 2!")))

  param_url <- format_URL_parameters(params)

  expect_equal(
    param_url,
    "param1=_Special (Symbols).,Check me 2!"
  )
})


test_that("Format URL parameters throws an error if not all list elements are named", {
  params <- list(
    param1 = "one",
    "two",
    "three"
  )

  expect_error(
    format_URL_parameters(params),
    "All components of query must be named"
  )
})