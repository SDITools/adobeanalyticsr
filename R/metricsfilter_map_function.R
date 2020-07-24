#' Build the metrics json list for report_freeform
#'
#' @export
#'
#'
addmetricsfilters <- function(metric,id2, filteridnumber = '0') {
    structure(list(
      columnId = id2,
      id = sprintf('metrics/%s',metric),
      filters = list(filteridnumber)
      ))
}



