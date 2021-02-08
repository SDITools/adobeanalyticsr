#' Used to format date and time
#'
#' @noRd
#'
#' @param start_date the first full day of data
#' @param end_date the last full day of data
#'
#'

make_timeframe <- function(start_date, end_date){
  #catch the date component when (character, date) are submitted as the date_range value
  if(!grepl('-', end_date)) {
    end_date<- as.Date(as.numeric(end_date),origin = "1970-01-01")
  }
  paste0(start_date, "T00:00:00.000/", end_date, "T23:59:59.999")
}