#' Get list of report suites
#'
#' @param company_id this can be either manually defined here or utilize the global definitions
#' @param rsids Filter list to only include suites in this RSID list (comma-delimited) Required
#' @param rsidContains Filter list to only include suites whose rsid contains (only one string at a time)
#' @param limit Number of results per page
#' @param page Page number (base 0 - first page is "0")
#' @param expansion options  c(name, parentRsid, currency, calendarType, timezoneZoneinfo)
#' @param debug Include the output and input of the api call in the console for debugging. Default is FALSE
#'
#'
#' @import stringr
#' @export
#'
aw_get_reportsuites <- function(company_id = Sys.getenv("AW_COMPANY_ID"),
                            rsids = '',
                            rsidContains = '',
                            limit = 10,
                            page = 0,
                            expansion = NA,
                            debug = FALSE)
  {

  #remove spaces from the list of expansion tags
  if(length(rsids) > 1) {rsids = URLencode(paste0(rsids, collapse = ',')) }
  if(length(rsidContains) > 1) {rsidContains = (paste0(rsidContains, collapse = ',')) }
  if(length(expansion) > 1) {expansion = paste(expansion, collapse = ',', sep = '') }

  #create the URL to send with the query
  urlstructure <- sprintf("collections/suites?rsids=%s&rsidContains=%s&limit=%s&page=%s&expansion=%s",
                 rsids,rsidContains,limit,page,expansion)

  res <- aw_call_api(req_path = urlstructure, debug = debug, company_id = company_id)

  res <- fromJSON(res)

  if (res$empty  == TRUE) {
    warning('No Report Suites weere returned!')
  } else {
  #Just need the content of the returned json
  res <- res$content

  res
  }
}
