#' Verbs available in segment rules.
#'
#' A dataset containing the list of available verbs to be used in segment rules.
#'
#' @format A data frame with 34 rows and 5 variables:
#' \describe{
#'   \item{type}{one of number, string, or exists}
#'   \item{class}{gives the context of the type of value is expected, either
#'   string, list, glob, number, or exists}
#'   \item{verb}{the actual verb id to be used in the segment defition}
#'   \item{description}{a simple description of the verb}
#'   \item{arg}{specifies what argument to use when building the segment verb function}
#'   ...
#' }
#' @source \url{https://developer.adobe.com/analytics-apis/docs/2.0/guides/endpoints/segments/definition/#available-data-comparison-functions}
"seg_verbs"

