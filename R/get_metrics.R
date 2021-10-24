#' Get list of metrics
#'
#' Get a data frame with all of the standard (non-calculated) metrics (measures) in the report suite.
#'
#' @param rsid Adobe report number
#' @param locale language - default'en_US'
#' @param segmentable - boolean - default NULL
#' @param expansion options  c('tags', 'allowedForReporting', 'categories'))
#' @param company_id define globally or manually
#' @param debug Include the output and input of the api call in the console for debugging. Default is FALSE
#' @param use_oob Always set to TRUE. Needed for tests
#'
#' @details
#' This function is commonly used to get the correct ID for a specific metric or metrics that will be used in
#' other function calls. The results returned are:
#'
#' * All of the "out of the box" metrics like visits, page views, visitors, orders, revenue, bounce rate, etc.
#'
#' * All of the enabled events that are configured in the report suite.
#'
#' * An instances metric for each enabled eVar.
#'
#' This function does _not_ return calculated metrics.
#'
#' @seealso \code{\link{aw_get_calculatedmetrics}}
#'
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used. Use \code{\link{aw_get_reportsuites}} to get a list of available `rsid` values.
#' @param locale The locale that system-named metrics should be returned in. Non-localized values will
#' be returned for title, name, description, etc. if a localized value is not available.
#' @param segmentable Boolean that determines whether or not to include metrics that can be used in
#' segments. `NULL` (the default) and `FALSE` return _all_ metrics (_not_ just the non-segmentable ones).
#' Examples of metrics that cannot be used in segments are `bounces`, `bounce rate`, `entries`, and `visitors`.
#' @param expansion Additional metrics metadata to include in the results: `tags`, `allowedForReporting`,
#' and `categories`. This argument takes a single value (e.g., `expansion = "tags"`) or a vector  of
#' values (e.g., `expansion = c("tags", "categories")`).
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use \code{\link{get_me}} to get a list of available `company_id` values.
#'
#' @return A data frame of metrics (excluding calculated metrics) and their meta data.
#'
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @export
aw_get_metrics <- function(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                           locale = 'en_US',
                           segmentable = 'NULL',
                           expansion = NA,
                           company_id = Sys.getenv("AW_COMPANY_ID"),
                           debug = FALSE){

  #remove spaces from the list of expansion tags
  if(is.na(paste(expansion,collapse=","))) {
    vars <- tibble::tibble(locale, segmentable)
  }
  if(!is.na(paste(expansion,collapse=","))) {
    vars <- tibble::tibble(locale, segmentable, expansion = paste(expansion,collapse=","))
  }

  #Turn the list into a string to create the query
  prequery <- list(vars %>% dplyr::select_if(~ any(!is.na(.))))

  #remove the extra parts of the string and replace it with the query parameter breaks
  query_param <- stringr::str_remove_all(stringr::str_replace_all(stringr::str_remove_all(paste(prequery, collapse = ''), '\\"'), ', ', '&'), 'list\\(| |\\)')

  #create the url to send with the query
  urlstructure <- glue::glue('metrics?rsid={rsid}&{query_param}')

  #request the metrics list from the API
  res <- aw_call_api(req_path = urlstructure,
                     debug = debug,
                     company_id = company_id)

  #change the result to a list
  res <- jsonlite::fromJSON(res)
  # removing "metrics/" from the beginning of the id value
  res$id <- stringr::str_sub(res$id, 9)

  res
}
