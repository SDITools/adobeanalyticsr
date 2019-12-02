#' Get list of dimensions
#'
#' @param rsid Adobe report number
#' @param locale language - default'en_US'
#' @param segmentable - boolean - default TRUE
#' @param reportable - boolean - default TRUE
#' @param classifiable - boolean - default FALSE
#' @param expansion options  c('tags', 'allowedForReporting', 'categories'))
#' @param company_id this can be either manually defined here or utilize the global definitions
#' @import stringr
#' @export
aa_get_dimensions <- function(rsid = Sys.getenv("AA_REPORTSUITE_ID"),
                              locale = 'en_US',
                              segmentable = 'false',
                              reportable = 'false',
                              classifiable = 'false',
                              expansion = 'tags,allowedForReporting,categories',
                              company_id = Sys.getenv("AA_COMPANY_ID") ){

  #remove spaces from the lsit of expansion tags
  expansion <- stringr::str_replace_all(expansion, fixed(" "), "")

  #create the url to send with the query
  urlstructure <- sprintf("dimensions?rsid=%s&locale=%s&segmentable=%s&reportable=%s&classifiable=%s&expansion=%s",
                 rsid,locale,segmentable,reportable,classifiable,expansion)

  res <- aa_get_elements(req_path = urlstructure)

  res <- fromJSON(res)
  # removing "metrics/" from the beginning of the id value
  res$id <- stringr::str_sub(res$id, 11)

  res
}
