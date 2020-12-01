#' Get list of metrics
#'
#' This will generate an extensive list of all the metrics (measures) in the reportsuite.
#'
#' @param rsid Adobe report number
#' @param locale language - default'en_US'
#' @param segmentable - boolean - default NULL
#' @param expansion options  c('tags', 'allowedForReporting', 'categories'))
#' @param company_id define globally or manually
#' @param debug Include the output and input of the api call in the console for debugging. Default is FALSE
#'
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @export
aw_get_metrics <- function(rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                              locale = 'en_US',
                              segmentable = 'NULL',
                              expansion = NA,
                              company_id = Sys.getenv("AW_COMPANY_ID"),
                              debug = FALSE){

  #remove spaces from the list of expansion tags
  if(is.na(paste(expansion,collapse=","))) {
    vars <- tibble::tibble(locale, segmentable)
  }
  if(!is.na(paste(expansion,collapse=","))) {
    vars <- tibble::tibble(locale, segmentable, expansion = paste(expansion,collapse=","))
  }

  #Turn the list into a string to create the query
  prequery <- list(vars %>% select_if(~ any(!is.na(.))))

  #remove the extra parts of the string and replace it with the query parameter breaks
  query_param <- stringr::str_remove_all(stringr::str_replace_all(stringr::str_remove_all(paste(prequery, collapse = ''), '\\"'), ', ', '&'), 'list\\(| |\\)')

  #create the url to send with the query
  urlstructure <- glue::glue('metrics?rsid={rsid}&{query_param}')

  #request the metrics list from the API
  res <- aw_call_api(req_path = urlstructure, debug = debug, company_id = company_id)

  #change the result to a list
  res <- jsonlite::fromJSON(res)
# removing "metrics/" from the beginning of the id value
  res$id <- stringr::str_sub(res$id, 9)

  res
}
