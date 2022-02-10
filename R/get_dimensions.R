#' Get list of dimensions
#'
#' This will generate an extensive list of all the dimensions in the reportsuite.
#'
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used. Use \code{\link{aw_get_reportsuites}} to get a list of available `rsid` values.
#' @param locale The locale that dimension details should be returned in. The default is `en_US`.
#' @param segmentable Boolean that determines whether or not to include dimensions that can be used in
#' segments. `FALSE` (the default) returns _all_ dimensions (_not_ just the non-segmentable ones).
#' Examples of dimensions that cannot be used in segments are `clickmapaction`, `codeversion`, `newvisit`,
#' and `pageurl`.
#' @param reportable Boolean that determines whether or not to include dimensions that can be used in
#' reports `FALSE` (the default) returns _all_ dimensions (_not_ just the non-segmentable ones).
#' @param classifiable Boolean that determines whether or not to include dimensions that can be used in
#' classifications `FALSE` (the default) returns _all_ dimensions (_not_ just the non-segmentable ones).
#' @param expansion Additional dimension metadata to include in the results: `tags`, `allowedForReporting`,
#' and `categories`. This argument takes a single value (e.g., `expansion = "tags"`) or a vector  of
#' values (e.g., `expansion = c("tags", "categories")`).
#' @param debug Include the output and input of the api call in the console for debugging. Default is FALSE
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use \code{\link{get_me}} to get a list of available `company_id` values.
#'
#' @return A data frame of dimensions and their meta data.
#'
#' @export
aw_get_dimensions <- function(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                              locale = 'en_US',
                              segmentable = FALSE,
                              reportable = FALSE,
                              classifiable = FALSE,
                              expansion = NULL,
                              debug = FALSE,
                              company_id = Sys.getenv("AW_COMPANY_ID") ){
  # Reference: https://adobedocs.github.io/analytics-2.0-apis/#/dimensions/dimensions_getDimensions

  query_params <- list(
    rsid = rsid,
    locale = locale,
    segmentable = segmentable,
    reportable = reportable,
    classifiable = classifiable,
    expansion = expansion
  )

  urlstructure <- paste("dimensions", format_URL_parameters(query_params), sep = "?")

  res <- aw_call_api(req_path = urlstructure, debug = debug, company_id = company_id)

  res <- jsonlite::fromJSON(res)

  # ID column returned with "variables/" prepended to names
  res$id <- gsub("^variables/", "", res$id)

  res
}
