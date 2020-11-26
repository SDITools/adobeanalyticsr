#' Get list of dimensions
#'
#' This will generate an extensive list of all the dimensions in the reportsuite.
#'
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used.
#' @param locale language - default'en_US'
#' @param segmentable - boolean - default TRUE
#' @param reportable - boolean - default TRUE
#' @param classifiable - boolean - default FALSE
#' @param expansion options  c('tags', 'allowedForReporting', 'categories'))
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' @import stringr
#' @export
aw_get_dimensions <- function(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                              locale = 'en_US',
                              segmentable = 'false',
                              reportable = 'false',
                              classifiable = 'false',
                              expansion = 'tags,allowedForReporting,categories',
                              company_id = Sys.getenv("AW_COMPANY_ID") ){

  #remove spaces from the lsit of expansion tags
  expansion <- stringr::str_replace_all(expansion,  stringr::fixed(" "), "")

  #create the url to send with the query
  urlstructure <- sprintf("dimensions?rsid=%s&locale=%s&segmentable=%s&reportable=%s&classifiable=%s&expansion=%s",
                 rsid,locale,segmentable,reportable,classifiable,expansion)

  res <- aw_call_api(req_path = urlstructure, company_id = company_id)

  res <- jsonlite::fromJSON(res)
  # removing "metrics/" from the beginning of the id value
  res$id <- stringr::str_sub(res$id, 11)

  res
}
