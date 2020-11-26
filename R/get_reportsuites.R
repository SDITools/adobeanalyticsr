#' Get list of report suites
#'
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' @param rsids Filter list to only include suites in this RSID list (comma-delimited) Required
#' @param rsidContains Filter list to only include suites whose rsid contains rsidContains
#' @param limit Number of results per page
#' @param page Page number (base 0 - first page is "0")
#' @param expansion options  c(name, parentRsid, currency, calendarType, timezoneZoneinfo))
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
                            expansion = 'name')
  {

  #remove spaces from the list of expansion tags
  expansion <- stringr::str_replace_all(expansion, fixed(" "), "")

  #create the URL to send with the query
  urlstructure <- sprintf("collections/suites?rsids=%s&rsidContains=%s&limit=%s&page=%s&expansion=%s",
                 rsids,rsidContains,limit,page,expansion)

  res <- aw_call_api(req_path = urlstructure, company_id = company_id)

  res <- fromJSON(res)

  if (res$empty  == TRUE) {
    warning('No Report Suites weere returned!')
  } else {
  #Just need the content of the returned json
  res <- res$content

  res
  }
}
