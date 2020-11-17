#' Get list of metrics
#'
#' This will generate an extensive list of all the metrics (measures) in the reportsuite.
#'
#' @param rsid Adobe report number
#' @param locale language - default'en_US'
#' @param segmentable - boolean - default NULL
#' @param expansion options  c('tags', 'allowedForReporting', 'categories'))
#' @param company_id define globally or manually
#'
#' @export
aw_get_metrics <- function(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                              locale = 'en_US',
                              segmentable = 'NULL',
                              expansion = 'tags, allowedForReporting, categories',
                              company_id = Sys.getenv("AW_COMPANY_ID")){

  #remove spaces from the lsit of expansion tags
  expansion <- stringr::str_replace_all(expansion,  stringr::fixed(" "), "")
  #create the url to send with the query
  urlstructure <- sprintf("metrics?rsid=%s&locale=%s&segmentable=%s&expansion=%s",
                                  rsid, locale, segmentable, expansion)



  res <- aw_call_api(req_path = urlstructure, company_id = company_id)

  res <- jsonlite::fromJSON(res)
# removing "metrics/" from the beginning of the id value
  res$id <- stringr::str_sub(res$id, 9)

  res
}
