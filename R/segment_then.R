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
#' Currently `pageviews` and `dimensions` are not supported unit values.
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
                     unit = 'year') {
#validation check for the limit argument
  #Check the spelling of the 2 variables
  for (i in 1:length(limit)) {
    if (!is.element(limit[i], c('within', 'after'))) {
      stop ("Make sure you have spelled `after` or `within` correctly.")
    }
  }
  #check that the count is a numeric
  assertthat::assert_that(is.numeric(count))

  if (length(limit) > 2 || length(count) > 2 || length(unit) > 2) {
    stop("Only 2 rules can be combined in a single sequence time restriction.")
  }
  if (length(limit) == 2 && sum(duplicated(limit)) == 1) {
    stop("The limit values cannot be the same. Use either 'within' and 'after' or only one.")
  }
  unit_options <- c('hit', 'visit', 'minute', 'hour', 'day', 'week', 'month', 'quarter', 'year')
  unit_name_test <- function(x) {
      if (!x %in% unit_options) {
        stop("The unit argument must be either: 'hit', 'visit', 'minute', 'hour', 'day', 'week', 'month', 'quarter', 'year'")
      } else { glue::glue("unit '{x}' is good")}
  }
  map(unit, unit_name_test)

  if (length(limit) == 2 && which(limit == 'within') == 2) {
    limit <- rev(limit)
    count <- rev(count)
    unit <- rev(unit)
  }
  #function to create the json if both after and within are used
  both_then <- function(count, limit, unit) {
    restriction <- list(
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
    for (i in 1:2) {
      if (length(restriction[[i]]$unit) > 0 && restriction[[i]]$unit == 'hit') {
        restriction[[i]]['unit'] <- NULL
        restriction[[i]]['func'] <- 'container-restriction'
      }
      if (!is.null(restriction[[i]]$unit) && restriction[[i]]$unit == 'visit') {
        restriction[[i]]['container'] <- 'visits'
        restriction[[i]]['unit'] <- NULL
        restriction[[i]]['func'] <- 'container-restriction'
      }
    }
    restriction
  }
  single_then <- function(count, limit, unit) {
    if (unit[[1]] == 'hit') {
      restriction <- list(count = count[[1]],
                          limit = limit[[1]],
                          container = 'hits',
                          func = 'container-restriction')
    } else if (unit[[1]] == 'visit') {
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
    restriction
  }
if (length(limit) == 2) {
  both_then(count, limit, unit)
} else {
  single_then(count, limit, unit)
}

}

