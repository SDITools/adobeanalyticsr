#' Create the segment sequence then object
#'
#' This function creates a then list object which restricts the time constraint of a segment to be added to a sequence segment.
#'
#' @param limit The limitation of the restriction. Either `within` (default) or `after`
#' @param count How many of the units should be used. `1` is set as default.
#' @param unit  A unit of time. Valid values are `hit`, `visit`, `minute`, `hour`, `day`, `week` (default), `month`, `quarter`, or `year`. Always use the singular form.
#'
#' @details
#' ## Combining `seg_then` arguments
#'
#' In the UI you can add 'after' and 'within' statements to create a more complex time restriction.
#' The same can be accomplished using this function by listing the limits, counts, and units in a c()
#' function. This would look like:  `limit = c('within', 'after'), count = c(5, 1), unit = c('hit', 'visit')`
#'
#' ## Using within and after in the same time `seg_then` function call
#'
#' Time restrictions can only be combined using 'within' first before 'after'.
#' The function will automatically align these to be in the correct list item order.
#'
#' ## A word about unit values
#'
#' In the UI `pageviews` and `dimensions` are options but they are not currently available in this function.
#'
#' @return a structured list of time restrictions to be used to build the sequential segment
#'
#' @import dplyr assertthat stringr
#'
#' @export
#'
#'
seg_then <- function(limit = 'within',
                     count =  1,
                     unit = 'week') {
#validation check for the limit argument
  if (length(limit) > 2 | length(count) > 2 | length(unit) > 2) {
    stop("Only 2 rules can be combined in a single sequence time restriction.")
  }
  if (length(limit) == 2 & sum(duplicated(limit)) == 1) {
    stop("The limit values cannot be the same. Use either ")
  }

  if (length(limit) == 2 & which(limit == 'within') == 2) {
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
  #check where units == 'hit'
  unithitlocation <- which(restriction == 'hit' & names(restriction) == 'unit')
  #remove the unit
  if(sum(unithitlocation) > 0){
    restriction <- restriction[-unithitlocation]
    restriction[which(names(restriction) == 'func')] <- 'container-restriction'
  }
  #check where units == 'visit'
  unitvisitlocation <- which(restriction == 'visit' & names(restriction) == 'unit')
  #remove the unit list item and change the container to 'visits'
  if(sum(unitvisitlocation) > 0){
    restriction[unitvisitlocation-1] <- 'visits'
    restriction <- restriction[-unitvisitlocation]
    restriction[which(names(restriction) == 'func')] <- 'container-restriction'
  }

} else {
  if(unit[[1]] == 'hit'){
  restriction <- list(count = count[[1]],
                      limit = limit[[1]],
                      container = 'hits',
                      func = 'container-restriction')
  } else if(unit[[1]] == 'visit'){
    restriction <- list(count = count[[1]],
                        limit = limit[[1]],
                        container = 'visits',
                        func = 'container-restriction')
  } else {
  restriction <- list(count = count[[1]],
                      limit = limit[[1]],
                      container = 'hits',
                      unit = unit[[1]],
                      func = 'time-restriction')
  }
}

return(restriction)
}

