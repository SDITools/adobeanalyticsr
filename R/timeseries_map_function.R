#' Build the metrics json list
#'
addtimeseriesmetrics <- function(metric,id2) {
    structure(list(
      columnId = as.character(id2),
      id = sprintf('metrics/%s',metric),
      filters = list('0') ))
}
