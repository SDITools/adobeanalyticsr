#' Get list of metrics
#'
#' Get a data frame with all of the standard (non-calculated) metrics (measures) in the report suite.
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
#' @seealso \code{\link{aw_get_calculatedmetrics}}, \code{\link{aw_get_calculatedmetric_byid}}
#'
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used.
#' @param locale The locale that system-named metrics should be returned in. Non-localized values will
#' be returned for title, name, description, etc. if a localized value is not available.
#' @param segmentable Boolean that determines whether or not to include metrics that can be used in
#' segments. `NULL` (the default) and `FALSE` return _all_ metrics (_not_ just the non-segmentable ones).
#' Examples of metrics that cannot be used in segments are `bounces`, `bounce rate`, `entries`, and `visitors`.
#' @param expansion Controls which of three specific columns to include in the results: `tags`, `allowedForReporting`,
#' and `categories`. The default is to include all three columns. Limit to one or any two by passing a
#' single value (e.g., `expansion = "tags"`) or a vector (e.g., `expansion = c("tags", "categories")`. This
#' argument has gotten way more explanation than warranted, as this capability seems relatively pointless
#' (but is available in the Adobe Analytics API, so it has been included).
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#'
#' @return A data frame of metrics (excluding calculated metrics) and their meta data.
#'
#' @export
aw_get_metrics <- function(rsid = Sys.getenv("AW_REPORTSUITE_ID"),

                              locale = 'en_US',
                              segmentable = 'NULL',
                              expansion = c('tags', 'allowedForReporting', 'categories'),
                              company_id = Sys.getenv("AW_COMPANY_ID")){

  #remove spaces from the list of expansion tags
  if(is.na(paste(expansion,collapse=","))) {
    vars <- tidyr::tibble(locale, segmentable)
  }
  if(!is.na(paste(expansion,collapse=","))) {
    vars <- tidyr::tibble(locale, segmentable, expansion = paste(expansion,collapse=","))
  }
  #Turn the list into a string to create the query
  prequery <- list(vars %>% select_if(~ any(!is.na(.))))
  #remove the extra parts of the string and replace it with the query parameter breaks
  query_param <- str_remove_all(str_replace_all(str_remove_all(paste(prequery, collapse = ''), '\\"'), ', ', '&'), 'list\\(| |\\)')

  #create the url to send with the query
  urlstructure <- glue::glue('metrics?rsid={rsid}&{query_param}')

  #request the metrics list from the API
  res <- aw_call_api(req_path = urlstructure, company_id = company_id)

  #change the result to a list
  res <- jsonlite::fromJSON(res)
  # removing "metrics/" from the beginning of the id value
  res$id <- stringr::str_sub(res$id, 9)

  res
}
