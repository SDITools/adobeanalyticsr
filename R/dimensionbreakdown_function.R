#' Build the breakdown_metrics json list for the metrics breakdown report - (sub-function)
#' @noRd
#'
breakdowns <- function(id,type,dimensions,itemId) {
  list(
      id = id,
      type = type,
      dimension = sprintf('variables/%s',dimensions),
      itemId = itemId
      )
 }
