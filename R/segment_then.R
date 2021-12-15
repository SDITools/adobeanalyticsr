#' Create the segment sequence then object
#'
#' This function creates a then list object which restricts the time constraint of a segment to be added to a sequence segment.
#'
#' @param limit The limitation of the restriction. Either `within` (default) or `after`
#' @param count How many of the units should be used
#' @param unit  A unit of time. Valid values are `minute`, `hour`, `day`, `week` (default), `month`, `quarter`, or `year`.
#'
#' @details
#'
#' **Using within and after in the same time restriction:**
#' Time restrictions can only be combined using 'within' first before 'after'.
#' The function will automatically align these to be in the correct list item order.
#'
#' @return a structured list of time restrictions to be used to build the sequential segment
#'
#' @import dplyr assertthat stringr
#'
#' @export
#'
#'
seg_then <- function(limit = 'within',
                            count = 1,
                            unit = 'week') {
#validation check for the limit argument
  if(length(limit) > 2 || length(count) > 2 || length(unit) > 2){
    stop("Only 2 rules can be combined in a single sequence time restriction.")
  }
  if(length(limit) == 2 && sum(duplicated(limit)) == 1){
    stop("The limit values cannot be the same. Use either ")
  }

  if(length(limit) == 2 && which(limit == 'within') == 2){
    limit <- rev(limit)
    count <- rev(count)
    unit <- rev(unit)
  }

if(length(limit) == 2){
  restriction <- c(
        list(count = count[[1]],
             limit = limit[[1]],
             container = 'hits',
             unit = unit[[1]],
             func = 'time-restriction'),
        list(count = count[[2]],
             limit = limit[[2]],
             container = 'hits',
             unit = unit[[2]],
             func = 'time-restriction')
  )
} else {
  restriction <- list(count = count[[1]],
                      limit = limit[[1]],
                      container = 'hits',
                      unit = unit[[1]],
                      func = 'time-restriction')
}

return(restriction)
}

