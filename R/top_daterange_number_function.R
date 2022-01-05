#' Automatically calculates date ranges
#'
#' If the end user enters '0' into the list of top argument values, the function
#' will inject the correct number of the date_range values that should be
#' pulled.
#'
#' Values passed to `top` are usually >0. If a daterange dimension corresponds
#' to a 0 in `top`, this is a special case, and the zero is expanded to the
#' number of units in `date_range`. This function expands these date values of
#' `top`.
#'
#' If the first dimension is a daterange, this results in an implied value of 0.
#' In this case, the length of `top` must only consider non-date dimensions,
#' and `length(top) == length(dimensions) - 1` holds. This feature is for
#' backwards compatibility with `RSiteCatalyst`. The value of top may also be
#' explicit.
#'
#' A better input data structure would be a pre-processed top argument where
#' there is no implied first value of top. I'll try to make this change and
#' then start writing unit tests for both.
#'
#' @noRd
#' @param top Character vector of the top (limit) numbers for the function argument 'top'
#' @param dimensions Character vector of the dimensions in the function argument 'dimensions'
#' @param date_range Character vector of the from and to dates in the function argument 'date_range'
#'
#' Other name ideas:
#' - recalculate_top_arg
#' - expand_top_zeros
#'
top_daterange_number <- function(top, dimensions, date_range) {
  if (!lubridate::is.POSIXt(date_range)) {
    date_range <- as.POSIXct(paste(date_range, c("00:00:00", "23:59:59")),
                             format = "%Y-%m-%d %H:%M:%S")
  }

  top <- make_explicit_top(top, dimensions)

  # For reference
  date_vars <- paste0("daterange",
                      c("minute", "hour", "day", "week", "month", "quarter", "year"))

  # These indices will apply to both dimensions and top
  datevar_inds <- (dimensions %in% date_vars) & top == 0

  # For each variable, calculate units
  ranges <- vapply(dimensions[datevar_inds], function(gran) {
    gran <- paste0(gsub("daterange", "", gran), "s")
    gran <- gsub("minute", "min", gran)
    d1 <- date_range[1]
    d2 <- date_range[2]

    .difftime2(d2,
               d1,
               units = gran)
  }, numeric(1))

  top[datevar_inds] <- as.integer(ceiling(ranges))
  top
}


#' Difftime, modified for queries
#'
#'
#'
#' @param d1 Date 1
#' @param d2 Date 2
#' @param units One of 'mins', 'hours', 'days', 'weeks', 'months', 'quarters', or 'years'
#'
#' @return The number of units. In the case of months, quarters, and years,
#'   returns the number of units that appear in the interval.
#'
#' @noRd
.difftime2 <- function(d1, d2, units) {
  if (units %in% c("mins", "hours", "days", "weeks")) {
    difftime(d1, d2, units = units)
  } else if (units %in% c("months", "quarters")) {
    day_seq <- seq(d2, d1, by = "days")

    # Get list of all months or quarters that appear in this range, plus year
    unit_u <- match.fun(units)(day_seq)   # Units (excludes year)
    unit_y <- lubridate::year(day_seq)    # Year
    unit_concat <- unique(paste(unit_u, unit_y))

    length(unit_concat)
  } else if (units == "years") {
    day_seq <- seq(d2, d1, by = "days")
    length(unique(lubridate::year(day_seq)))
  } else {
    stop("Units must be one of 'mins', 'hours', 'days', 'weeks', 'months', 'quarters', or 'years'")
  }
}


#' Expand an explicit top argument
#'
#'
#' @details
#' The value of `top` usually must be explicit, but in the case of dates it may
#' be implied. There are two cases:
#'
#' 1. If a daterange dimension has a corresponding `top` value of 0, then the
#' value will be expanded to be the number of units in the date range.
#' 2. If a daterange dimension is the first dimension, it takes an implied value
#' of 0, in which case `length(top) == length(dimensions) - 1`.
#'
#' This function handles the second case, but not the first. `top` also may be
#' a vector of length 1, in which case it is recycled to match the length of
#' `dimensions`. If `top` is length 1 and the first dimension is a date
#' dimension, then a 0 is inserted before recycling, to preserve the implied 0.
#'
#' @param top Number of each dimension to return
#' @param dimensions Dimensions that each value of top pertains to
#'
#' @return Named character vector where the name is the dimension and the
#' value is the number of items to return
#'
make_explicit_top <- function(top, dimensions) {
  date_dimensions <- paste0("daterange",
                           c("minute",
                             "hour",
                             "day",
                             "week",
                             "month",
                             "quarter",
                             "year"))

  # Recycle 'top', with first-position date dimension rule
  if (length(top) == 1 & length(dimensions) > 1) {
    top <- rep(top, times = length(dimensions))

    if (dimensions[1] %in% date_dimensions) top[1] <- 0
  }

  # Introduce implied 0 when length(top) == length(dimensions) - 1
  if (length(top) != length(dimensions)) {
    valid_exception_conditions <- c(
      length(top) == length(dimensions) - 1,
      dimensions[1] %in% date_dimensions
    )

    if (!all(valid_exception_conditions)) stop("Invalid combination of 'top' and 'dimensions'")

    top <- c(0, top)
  }

  setNames(top, dimensions)
}