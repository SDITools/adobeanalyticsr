#' Create the Segment Container
#'
#' This function combines predicates into a container.
#'
#' @param context One of hits, visits, or visitors. Also known as scope
#' @param conjunction  How should the predicates be combined?  And is the default.
#' @param predicates List of predicates created using `seg_pred()` function. Must wrapped in a list() function.
#' @param exclude Exclude the entire container
#'
#' @return a structured list of containers to be used to build the segment
#'
#' @import dplyr assertthat stringr
#'
#' @export
#'
#'
seg_con <- function(context = 'hits',
                    conjunction = 'and',
                    predicates = NULL,
                    exclude = FALSE) {
  if(length(unlist(predicates)) > 3 & exclude == FALSE) {
    structure(list(
      func = 'container',
      context = context,
      pred = list(
        func = conjunction,
        preds = predicates
      )
    ))
  } else if(length(unlist(predicates)) > 3 & exclude == TRUE) {
    structure(list(
      func = 'container',
      context = context,
      pred = list(
        func = 'without',
        pred = list(
          func = conjunction,
          preds = predicates
          )
        )
    ))
  } else if (length(unlist(predicates)) < 3 & exclude == FALSE) {
    structure(list(
      func = 'container',
      context = context,
      pred =  predicates
    ))
  } else if (length(unlist(predicates)) < 3 & exclude == TRUE) {
    structure(list(
      func = 'without',
      pred = list(
        func = 'without',
        pred = list(
          func = 'container',
          context = context,
          pred =  predicates
        )
      )
    ))
  }
}

