#' Get list of dimensions
#'
#' This will generate an extensive list of all the dimensions in the reportsuite.
#'
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used.
#' @param locale The locale that dimension details should be returned in. The default is `en_US`.
#' @param segmentable Boolean that determines whether or not to include dimensions that can be used in
#' segments. `FALSE` (the default) returns _all_ dimensions (_not_ just the non-segmentable ones).
#' Examples of dimensions that cannot be used in segments are `clickmapaction`, `codeversion`, `newvisit`,
#' and `pageurl`.
#' @param reportable Boolean that determines whether or not to include dimensions that can be used in
#' reports `FALSE` (the default) returns _all_ dimensions (_not_ just the non-segmentable ones).
#' @param classifiable Boolean that determines whether or not to include dimensions that can be used in
#' classifications `FALSE` (the default) returns _all_ dimensions (_not_ just the non-segmentable ones).
#' @param expansion Comma-delimited list of additional segment metadata fields to include in
#' the results. Valid values include: `tags`, `allowedForReporting`, and `categories`. To include multiple values,
#' combine them into a single comma-separated string with _no spaces_.
#' @param debug Include the output and input of the api call in the console for debugging. Default is FALSE
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#'
#' @return A data frame of dimensions and their meta data.
#'
#' @import stringr
#' @export
aw_get_dimensions <- function(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                              locale = 'en_US',
                              segmentable = FALSE,
                              reportable = FALSE,
                              classifiable = FALSE,
                              expansion = NA,
                              debug = FALSE,
                              company_id = Sys.getenv("AW_COMPANY_ID") ){

  #remove spaces from the list of expansion tags
  if(is.na(paste(expansion,collapse=","))) {
    vars <- tibble::tibble(locale, segmentable)
  }
  if(!is.na(paste(expansion,collapse=","))) {
    vars <- tibble::tibble(locale, segmentable, reportable, classifiable, expansion = paste(expansion,collapse=","))
  }
  #Turn the list into a string to create the query
  prequery <- list(vars %>% dplyr::select_if(~ any(!is.na(.))))
  #remove the extra parts of the string and replace it with the query parameter breaks
  query_param <- stringr::str_remove_all(stringr::str_replace_all(stringr::str_remove_all(paste(prequery, collapse = ''), '\\"'), ', ', '&'), 'list\\(| |\\)')

  #create the url to send with the query
  urlstructure <-  glue::glue('dimensions?rsid={rsid}&{query_param}')

  res <- aw_call_api(req_path = urlstructure, debug = debug, company_id = company_id)

  res <- jsonlite::fromJSON(res)

  # removing "metrics/" from the beginning of the id value
  res$id <- stringr::str_sub(res$id, 11)

  res
}
