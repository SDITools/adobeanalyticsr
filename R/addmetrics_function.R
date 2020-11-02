#' Build the metrics json list for report_freeform - (sub-function)
#' @noRd
#' @export
#'
addmetrics <- function(metric,id2,metricSort) {
  if(id2 == 0 & !is.null(metricSort)) {
    structure(list(
      columnId = id2,
      id = sprintf('metrics/%s',metric),
      sort = metricSort))
  } else {
    structure(list(columnId = id2,
                   id = sprintf('metrics/%s',metric)))
  }}
