#' Retrieve a list of verbs for segments
#'
#' This function generates a list of linking verbs that can be used to build segments
#'
#' @return A list of all available verbs
#'
#'
#' @export
#'
seg_verbs <- function(){
  verbs <- structure( list(
    Exists = c('exists', 'not-exists', 'event-exists', 'not-event-exists'),
    Strings = list(str_verbs = c('contains', 'not-contains', 'starts-with', 'ends-with', 'not-starts-with', 'not-ends-with', 'streq', 'not-streq', 'strlt', 'strgt', 'strle', 'strge'), list_verbs = c('streq-in', 'not-streq-in', 'contains-any-of', 'contains-all-of', 'not-contains-any-of', 'not-contains-all-of'), glob_verbs = c('matches', 'not-match')),
    Numbers = list(num_verbs = c('eq', 'not-eq', 'gt', 'lt', 'ge', 'le'),
                  numlist_verbs = c('eq-any-of', 'not-eq-any-of', 'eq-in', 'not-eq-in')) ))
  return(verbs)
}