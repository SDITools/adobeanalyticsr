#' Get list of dimensions
#'
#' This will generate an extensive list of all the dimensions in the reportsuite.
#'
#' @param rsid Adobe report number - required
#' @param locale language - default'en_US'
#' @param segmentable - boolean - default FALSE
#' @param reportable - boolean - default FALSE
#' @param classifiable - boolean - default FALSE
#' @param expansion options  c('tags', 'allowedForReporting', 'categories')
#' @param debug Include the output and input of the api call in the console for debugging. Default is FALSE
#' @param company_id this can be either manually defined here or utilize the global definitions
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
