#' Get list of dimensions
#'
#' @param rsid Adobe report number
#' @param locale language - default'en_US'
#' @param segmentable - boolean - default TRUE
#' @param expansion options  c('tags', 'allowedForReporting', 'categories'))
#'
#' @export
aa_get_metrics <- function(rsid = Sys.getenv("AA_RSID"),
                              locale = 'en_US',
                              segmentable = 'false',
                              expansion = c('tags', 'allowedForReporting', 'categories')){
  #create the url to send with the query

  urlstructure <- sprintf(rsid,locale,segmentable)

  #&segmentable=%s&reportable=%s&classifiable=%s



  res <- aa_get_elements(req_path = urlstructure)

  res <- fromJSON(res)
# removing "metrics/" from the beginning of the id value
  res$id <- stringr::str_sub(res$id, 9)

  res
}
