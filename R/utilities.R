#' make_timeframe function for formating date and time
#' @export
#'

make_timeframe <- function(start_date, end_date){
  paste0(start_date, "T00:00:00.000/", end_date, "T23:59:59.999")
}
