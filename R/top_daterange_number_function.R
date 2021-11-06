#' Automatically calculates date ranges
#'
#' If the end user enters '0' into the list of top argument values, the function will inject the correct number of
#' the date_range values that should be pulled.
#' @noRd
#' @param top Character vector of the top (limit) numbers for the function argument 'top'
#' @param dimensions Character vector of the dimensions in the function argument 'dimensions'
#' @param date_range Character vector of the from and to dates in the function argument 'date_range'
#'
top_daterange_number <- function(top, dimensions, date_range) {
  if (!lubridate::is.POSIXt(date_range)) {
    date_range <- as.POSIXct(paste(date_range, c("00:00:00", "23:59:59")),
                             format = "%Y-%m-%d %H:%M:%S")
  }

  if(length(top) != length(dimensions) & length(top) != 1) {
    stop('TOP length: The "top" number of values must be equal the length of the "dimensions" list or 1 unless the first dimension is a "daterange" metric in which case the number of "top" items only has to match the length of the non "daterange" items.')
  } else if(grepl('daterange', dimensions[1]) & length(top) == 1) {
    top <- rep(top, length(dimensions)-1)
    top <- c(0, top)
  } else if(length(top) == 1) {
    top <- rep(top, length(dimensions))
  }

  # For reference
  date_vars <- paste0("daterange",
                      c("minute", "hour", "day", "week", "month", "quarter", "year"))

  # These indices will apply to both dimensions and top
  datevar_inds <- (dimensions %in% date_vars) & top == 0

  # For each variable, calculate units
  ranges <- vapply(dimensions[datevar_inds], function(gran) {
    gran <- paste0(gsub("daterange|ute", "", gran), "s")
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

