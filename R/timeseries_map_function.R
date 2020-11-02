#' Build the timeseries metrics json list
#' @noRd
#' @param metric The metric to add to the json list
#' @param id2 The number of items to add as the columnId
#'
addtimeseriesmetrics <- function(metric,id2) {
    structure(list(
      columnId = as.character(id2),
      id = sprintf('metrics/%s',metric),
      filters = list('0') ))
}
