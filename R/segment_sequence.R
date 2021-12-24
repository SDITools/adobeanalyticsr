#' Create the Segment Sequence Container
#'
#' This function combines predicates into a sequence container.
#'
#' @param context One of hits, visits, or visitors. Also known as scope
#' @param predicates List of predicates created using `seg_pred()` function. Must wrapped in a list() function.
#' @param sequence  How should the sequence of items be considered. Options: `in_order` (default), `before`, `after`, `and`, `or`
#' @param exclude Excludes the entire sequence container which will include all predicates.
#' @param exclude_checkpoint Which checkpoints (predicates) should be excluded.
#'
#' @details
#'
#' Exclude checkpoint: Ensures the next checkpoint doesn't happen between the
#' preceding checkpoint and the subsequent checkpoint. If there is no subsequent
#' checkpoint then the excluded checkpoint must not occur at any point after
#' the preceding checkpoint. If there is no preceding checkpoint then the
#' excluded checkpoint must not have occurred at any point preceding the
#' subsequent checkpoint.
#'
#' @return a structured list of containers to be used to build the segment
#'
#' @import dplyr assertthat stringr
#'
#' @export
#'
#'
seg_seq <- function(context = 'visits',
                    predicates = NULL,
                    sequence = 'in_order',
                    exclude = FALSE,
                    exclude_checkpoint = NULL) {

sequence_dir <- dplyr::case_when(sequence == 'in_order' ~ 'sequence',
                                 sequence == 'after' ~ 'sequence-prefix',
                                 sequence == 'before' ~ 'sequence-suffix',
                                 sequence == 'and' ~ 'sequence-and',
                                 sequence == 'or' ~ 'sequence-or')

## Append the exclusion list item
if(!is.null(exclude_checkpoint)) {
  for(i in seq(exclude_checkpoint)) {
    exchecks <- seq(exclude_checkpoint)-1
  predicates <- append(predicates,
                       list(list(func = 'exclude-next-checkpoint')),
                       exclude_checkpoint[[i]]+exchecks[[i]]-1
                       )
  }
}
## Add in the necessary 'container' and 'hits' variables to each predicate for sequence to work
pred_items <- list()
for (i in seq_along(predicates)) {
  if (!is.null(predicates[[i]]$val)) {
    pred_items <- append(pred_items, list(
      list(context = 'hits',
           func = 'container',
           pred = predicates[[i]])
    ))
  } else if (length(predicates[[i]]) == 2) {
    if (is.list(predicates[[i]][[1]])){
      for(item in seq(length(predicates[[i]]))) {
        pred_items <- append(pred_items, predicates[[i]][item])
      } }
  } else {
    pred_items <- append(pred_items, list(predicates[[i]]))
  }
}

sequence_items <- if(sequence_dir == 'sequence') {
  if(exclude == FALSE) {
    structure(
      list(
        func = sequence_dir,
        stream = pred_items
      )
    )
  } else if(exclude == TRUE){
    structure(
      list(
        func = 'without',
        pred = list(
          func = sequence_dir,
          stream = pred_items
        )
      )
    )
  }
} else if(sequence_dir %in% c('sequence-prefix', 'sequence-suffix')) {
  if(exclude == FALSE) {
    structure(
      list(
        func = sequence_dir,
        context = context,
        stream = pred_items
      )
    )
  } else if(exclude == TRUE) {
    structure(
      list(
        func = 'without',
        pred = list(
          func = sequence_dir,
          context = context,
          stream = pred_items
        )
      )
    )
  }
} else if(sequence_dir %in% c('sequence-and', 'sequence-or')) {
  if(exclude == FALSE) {
    structure(
      list(
        func = sequence_dir,
        checkpoints = pred_items
      )
    )
  } else if(exclude == TRUE){
    structure(
      list(
        func = 'without',
        pred = list(
          func = sequence_dir,
          checkpoints = pred_items
        )
      )
    )
  }
}

return(sequence_items)
}

