#' Create the Segment Container
#'
#' This function combines predicates into a container.
#'
#' @param context Defines the level that the segment logic should operate on. Valid values are visitors, visits, and hits. See Details
#' @param conjunction  This defines the relationship of the predicates.  `And` (default) and `or` are the two options.
#' @param predicates List of predicates and/or containers. Must be wrapped in a list() function. Adding a container list item will nest it within a containers.
#' @param exclude Exclude the entire container
#'
#' @details
#'
#' **Context**
#' The rules in a segment have a context that specify the level of operation. The context can be visitors, visits or hits.
#' As an example, let's build a segment rule where revenue is greater than 0 (meaning a purchase took place) and change the context to see how things change.
#' If the context is set to visitors, the segment includes all hits from visitors that have a purchase of some kind during a visit. This is useful in analyzing customer behavior in visits leading up to a purchase and possibly behavior after a purchase.
#' the context is set to visits, the segment includes all hits from visits where a purchase occurred. This is useful for seeing the behavior of a visitor in immediate page views leading up to the purchase.
#' If the context is set to hit, the segment only includes hits where a purchase occurred, and no other hits. This is useful in seeing which products were most popular.
#' In the above example, the context for the container listed is hits. This means that the container only evaluates data at the hit level, (in contrast to visit or visitor level). The rows in the container are also at the hit level.
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
container <- if (length(unlist(predicates)) > 3 && exclude == FALSE) {
    structure(list(
      func = 'container',
      context = context,
      pred = list(
        func = conjunction,
        preds = predicates
      )
    ))
  } else if (length(unlist(predicates)) > 3 && exclude == TRUE) {
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
  } else if (length(unlist(predicates)) < 3 && exclude == FALSE) {
    structure(list(
      func = 'container',
      context = context,
      pred =  predicates
    ))
  } else if (length(unlist(predicates)) < 3 && exclude == TRUE) {
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
 ## add in the "sessions" context value if the allocation model is not null
  for (i in length(container$pred$preds)) {
    if (!is.null(container$pred$preds[[i]]$val$`allocation-model`$func) && context == 'visits') {
      container$pred$preds[[i]]$val$`allocation-model`$context = 'sessions'
    }
  }
container
}

