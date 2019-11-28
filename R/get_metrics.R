#' Get list of dimensions
#'
#' @param rsid Adobe report number
#' @param locale language - default'en_US'
#' @param segmentable - boolean - default TRUE
#' @param expansion options  c('tags', 'allowedForReporting', 'categories'))
#'
#' @export
aa_get_metrics <- function(rsid = Sys.getenv("AA_REPORTSUITE_ID"),
                              locale = 'en_US',
                              segmentable = 'false',
                              expansion = 'tags, allowedForReporting, categories',
                              company_id = Sys.getenv("AA_COMPANY_ID")){

  #remove spaces from the lsit of expansion tags
  expansion <- stringr::str_replace_all(expansion, fixed(" "), "")
  #create the url to send with the query
  urlstructure <- sprintf("metrics?rsid=%s&locale=%s&segmentable=%s&expansion=%s",
                                  rsid, locale, segmentable, expansion)



  res <- aa_get_elements(req_path = urlstructure)

  res <- fromJSON(res)
# removing "metrics/" from the beginning of the id value
  res$id <- stringr::str_sub(res$id, 9)

  res
}
