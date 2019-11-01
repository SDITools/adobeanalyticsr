#' Build the breakdown_metrics json list for the metrics breakdown report
#'
#' @export
#'
breakdowns <- function(id,type,dimensions,itemId) {
    structure(list(
      id = id,
      type = type,
      dimension = sprintf('variables/%s',dimensions),
      itemId = itemId
      ))
 }
