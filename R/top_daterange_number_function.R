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

  if(length(top) != length(dimensions) & length(top) != 1) {
    stop('TOP length: The "top" number of values must be equal the length of the "dimensions" list or 1 unless the first dimension is a "daterange" metric in which case the number of "top" items only has to match the length of the non "daterange" items.')
  } else if(grepl('daterange', dimensions[1]) & length(top) == 1) {
    top <- rep(top, length(dimensions)-1)
    top <- c(0, top)
  } else if(length(top) == 1) {
    top <- rep(top, length(dimensions))
  }

  chck <- cbind(dimensions, top)
  mns <- as.numeric(difftime(as.Date(date_range[2])+1, as.Date(date_range[1]), units = 'mins'))
  hrs <- as.numeric(difftime(as.Date(date_range[2])+1, as.Date(date_range[1]), units = 'hours'))
  dys <- length(seq.Date(as.Date(date_range[1]), as.Date(date_range[2]), by = 'day'))
  wks <- length(seq.Date(as.Date(date_range[1]), as.Date(date_range[2]), by = 'week'))
  mts <- length(seq.Date(as.Date(date_range[1]), as.Date(date_range[2]), by = 'month'))
  qtrs <- length(seq.Date(as.Date(date_range[1]), as.Date(date_range[2]), by = 'quarter'))
  yrs <- length(seq.Date(as.Date(date_range[1]), as.Date(date_range[2]), by = 'year'))

  chck[,2][chck[,2] == 0 & chck[,1] == 'daterangeminute'] <- mns
  chck[,2][chck[,2] == 0 & chck[,1] == 'daterangehour'] <- hrs
  chck[,2][chck[,2] == 0 & chck[,1] == 'daterangeday'] <- dys
  chck[,2][chck[,2] == 0 & chck[,1] == 'daterangeweek'] <- wks
  chck[,2][chck[,2] == 0 & chck[,1] == 'daterangemonth'] <- mts
  chck[,2][chck[,2] == 0 & chck[,1] == 'daterangequarter'] <- qtrs
  chck[,2][chck[,2] == 0 & chck[,1] == 'daterangeyear'] <- yrs

  top = c(chck[,2])
  top
}
