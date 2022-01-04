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
#' @export
aw_get_metrics <- function(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                           locale = 'en_US',
                           segmentable = 'NULL',
                           expansion = NULL,
                           company_id = Sys.getenv("AW_COMPANY_ID"),
                           debug = FALSE,
                           use_oob = TRUE){
  # Reference: https://adobedocs.github.io/analytics-2.0-apis/#/metrics/getMetrics
  lifecycle::deprecate_warn("0.2.2", what = "aw_get_metrics(use_oob)")

  assertthat::assert_that(
    assertthat::is.string(rsid)
  )

  query_params <- list(
    rsid = rsid,
    locale = locale,
    segmentable = segmentable,
    expansion = expansion
  )

  urlstructure <- paste("metrics", format_URL_parameters(query_params), sep = "?")

  res <- aw_call_api(req_path = urlstructure,
                     debug = debug,
                     company_id = company_id)

  res <- jsonlite::fromJSON(res)

  # The ID values is returned with "metrics/" prepended to it
  res$id <- gsub("^metrics/", "", res$id)

  res
}
