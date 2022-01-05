#' Convert dates to API (ISO 8601) format
#'
#' @description
#' Adobe Analytics dates have a timezone set by the implementation, so all
#' dates are given in local time by default. This function converts dates
#' to a generic ISO 8601 format for the API. POSIX times are left unchanged.
#'
#'
#' @details
#' Strings and numerics are converted to dates. Numeric values are ambiguous
#' (they could be dates or datetimes). They are converted automatically to dates
#' with an origin of [lubridate::origin] with a warning.
#'
#' @param dates Vector of dates, of class numeric, Date, or POSIXt
#' @param origin Origin to use for numeric values
#'
#' @return String, date range to include in query
#' @noRd
#'
#' @examples
#' dates_str <- c('2021-01-01', '2021-01-10')
#' dates_date <- as.Date(c("2021-01-01", "2021-01-10"))
#' posix <- as.POSIXct(c("2021-01-01T12:00:00", "2021-01-10T15:59:59"), format = "%Y-%m-%dT%H:%M:%S")
#'
#' make_timeframe(dates_str)
#' make_timeframe(dates_date)
#' make_timeframe(posixs)
make_timeframe <- function(dates, origin = lubridate::origin) {
  stopifnot(length(dates) == 2)

  if (is.numeric(dates)) {
    warning("Numeric values for date range will be converted to dates")
  }

  # Change posix to date formats
  if (lubridate::is.POSIXt(dates)) {
    return(paste(
      lubridate::format_ISO8601(dates[1], usetz = FALSE),
      lubridate::format_ISO8601(dates[2], usetz = FALSE),
      sep = "/"
    ))
  }

  # Check if is character format i.e. '2021-01-01'
  if (is.character(dates)) {
    dates <- as.Date(dates)
  }

  dates <- as.Date(dates, origin)

  start <- paste0(as.character(dates[1]), "T00:00:00")
  end <- paste0(as.character(dates[2]), "T23:59:59")

  paste(start, end, sep = "/")
}

# Set the timeframe for the query (timeframe)
# Used by get_usage_logs()
make_startDate_endDate <- function(start_date, end_date){
  if(!grepl('-', start_date)) {
    end_date<- as.Date(as.numeric(end_date),origin = "1970-01-01")
  }
  list(glue::glue('{start_date}T00:00:00.000'), glue::glue('{end_date}T23:59:59.999'))
}


#' Fill a vector of a certain length with NA
#'
#' Similar to [vctrs::vec_recycle()], but fills remaining values with `NA`.
#'
#' @param x Vector
#' @param len Length to fill to
#'
#' @return A vector the same length as `len` with the difference made up by `NA`
#' @noRd
na_fill_vec <- function(x, len) {
  len_x <- length(x)
  if (len_x != len & len_x != 1) {
    stop("Vector has length !=1 but not `len`")
  } else if (len_x == len) {
    return(x)
  } else if (len_x == 1) {
    x[2:len] <- NA
  }

  x
}
