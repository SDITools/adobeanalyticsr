#' Get list of reportsuites
#'
#' @param company_id this can be either manually defined here or utilize the global definitions
#' @param rsids Filter list to only include suites in this RSID list (comma-delimited)
#' @param rsidContains Filter list to only include suites whose rsid contains rsidContains
#' @param limit Number of results per page
#' @param page Page number (base 0 - first page is "0")
#' @param expansion options  c(name, parentRsid, currency, calendarType, timezoneZoneinfo))
#'
#'
#' @import stringr
#' @export
#'
get_reportsuites <- function(company_id = Sys.getenv("AA_COMPANY_ID"),
                            rsids = '',
                            rsidContains = '',
                            limit = 10,
                            page = 0,
                            expansion = 'name')
  {

  #remove spaces from the lsit of expansion tags
  expansion <- stringr::str_replace_all(expansion, fixed(" "), "")

  #create the url to send with the query
  urlstructure <- sprintf("collections/suites?rsids=%s&rsidContains=%s&limit=%s&page=%s&expansion=%s",
                 rsids,rsidContains,limit,page,expansion)

  res <- aa_call_api(req_path = urlstructure, company_id = company_id)

  res <- fromJSON(res)

  if (res$empty  == TRUE) {
    warning('No Report Suites weere returned!')
  } else {
  #Just need the content of the returned json
  res <- res$content

  res
  }
}
