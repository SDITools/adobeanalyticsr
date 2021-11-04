#' Automatically calculates date ranges
#'
#' If the end user enters '0' into the list of top argument values, the function will inject the correct number of
#' the date_range values that should be pulled.
#' @noRd
#' @param top Character vector of the top (limit) numbers for the function argument 'top'
#' @param dimensions Character vector of the dimensions in the function argument 'dimensions'
#' @param date_range Character vector of the from and to dates in the function argument 'date_range'
#'
# top_daterange_number <- function(top, dimensions, date_range) {
#
#   if(length(top) != length(dimensions) & length(top) != 1) {
#     stop('TOP length: The "top" number of values must be equal the length of the "dimensions" list or 1 unless the first dimension is a "daterange" metric in which case the number of "top" items only has to match the length of the non "daterange" items.')
#   } else if(grepl('daterange', dimensions[1]) & length(top) == 1) {
#     top <- rep(top, length(dimensions)-1)
#     top <- c(0, top)
#   } else if(length(top) == 1) {
#     top <- rep(top, length(dimensions))
#   }
#
#   if (!lubridate::is.POSIXt(date_range)) {
#     date_range <- as.POSIXct(paste(date_range, c("00:00:00", "23:59:59")),
#                              format = "%Y-%m-%d %H:%M:%S")
#   }
#
#   chck <- cbind(dimensions, top)
#   mns <- as.numeric(difftime(date_range[2], date_range[1], units = 'mins'))
#   hrs <- as.numeric(difftime(as.Date(date_range[2])+1, as.Date(date_range[1]), units = 'hours'))
#   dys <- length(seq.Date(as.Date(date_range[1]), as.Date(date_range[2]), by = 'day'))
#   wks <- length(seq.Date(as.Date(date_range[1]), as.Date(date_range[2]), by = 'week'))
#   mts <- length(seq.Date(as.Date(date_range[1]), as.Date(date_range[2]), by = 'month'))
#   qtrs <- length(seq.Date(as.Date(date_range[1]), as.Date(date_range[2]), by = 'quarter'))
#   yrs <- length(seq.Date(as.Date(date_range[1]), as.Date(date_range[2]), by = 'year'))
#
#   chck[,2][chck[,2] == 0 & chck[,1] == 'daterangeminute'] <- mns
#   chck[,2][chck[,2] == 0 & chck[,1] == 'daterangehour'] <- hrs
#   chck[,2][chck[,2] == 0 & chck[,1] == 'daterangeday'] <- dys
#   chck[,2][chck[,2] == 0 & chck[,1] == 'daterangeweek'] <- wks
#   chck[,2][chck[,2] == 0 & chck[,1] == 'daterangemonth'] <- mts
#   chck[,2][chck[,2] == 0 & chck[,1] == 'daterangequarter'] <- qtrs
#   chck[,2][chck[,2] == 0 & chck[,1] == 'daterangeyear'] <- yrs
#
#   top = c(chck[,2])
#   as.numeric(top)
# }
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

.difftime2 <- function(d1, d2, units) {
  if (units %in% c("mins", "hours", "days", "weeks")) {
    difftime(d1, d2, units = units)
  } else if (units %in% c("months", "quarters")) {
    length(unique(match.fun(units)(c(d1, d2))))
  } else if (units == "years") {
    length(unique(lubridate::year(c(d1, d2))))
  } else {
    stop("Units must be one of 'mins', 'hours', 'days', 'weeks', 'months', 'quarters', or 'years'")
  }
}

#
# just_dates <- as.Date(c("2021-01-01", "2021-01-10"))
# just_datetimes <- as.POSIXct(c("2021-10-01 13:00:00",
#                                "2021-10-01 15:00:00"),
#                              format = "%Y-%m-%d %H:%M:%S")
#
# top_daterange_number2(5, c("daterangehour", "prop45"), just_datetimes)
# top_daterange_number2(c(5, 5), c("daterangehour", "prop45"), just_dates)
# top_daterange_number2(5, c("daterangeyear", "prop45"), just_dates)
