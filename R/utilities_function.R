#' Used to format date and time
#'
#' @noRd
#'
#' @param start_date the first full day of data
#' @param end_date the last full day of data
#'
#' @export
#'

make_timeframe <- function(start_date, end_date){
  paste0(start_date, "T00:00:00.000/", end_date, "T23:59:59.999")
}