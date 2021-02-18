#' Get a list of user usage
#'
#' This function returns the usage and access logs for a given date range within a 3 month period.
#'
#' @param startDate Start date for the maximum of a 3 month period.
#' @param endDate End date for the maximum of a 3 month period.
#' @param login The login value of the user you want to filter logs by.
#' @param ip The IP address you want to filter logs by.
#' @param rsid The report suite ID you want to filter logs by.
#' @param eventType The numeric id for the event type you want to filter logs by.
#' @param event The event description you want to filter logs by. No wildcards permitted.
#' @param limit The number of results to return per page. This argument works in conjunction with the
#' `page` argument. The default is 10.
#' @param page The "page" of results to display. This works in conjunction with the `limit` argument and is
#' zero-based. For instance, if `limit = 20` and `page = 1`, the results returned would be 21 through 40.
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use \code{\link{get_me}} to get a list of available `company_id` values.
#'
#' @return A data frame of users and their meta data.
#' @examples
#' \dontrun{
#' get_usage(startDate = Sys.Date()-91, endDate = Sys.Date()-1, limit = 100, page = 0)
#' }
#'
#' @import stringr
#' @export
#'
get_usage <- function(startDate = Sys.Date()-91,
                      endDate = Sys.Date()-1,
                      login = NA,
                      ip = NA ,
                      rsid = NA,
                      eventType = NA,
                      event = NA,
                      limit = 100,
                      page = 0,
                      company_id = Sys.getenv("AW_COMPANY_ID")
                      )
{

  vars <- tibble::tibble(login, ip, rsid, eventType, event, limit, page)
  #Turn the list into a string to create the query
  prequery <- list(dplyr::select_if(vars, ~ !any(is.na(.))))
  #remove the extra parts of the string and replace it with the query parameter breaks
  query_param <- stringr::str_remove_all(stringr::str_replace_all(stringr::str_remove_all(paste(prequery, collapse = ''), '\\"'), ', ', '&'), 'list\\(| |\\)')

  #create the url to send with the query
  urlstructure <- paste0('users?',query_param)

  #urlstructure <- 'segments?locale=en_US&filterByPublishedSegments=all&limit=1000&page=0&sortDirection=ASC&sortProperty=id&includeType=all'
  res <- aw_call_api(req_path = urlstructure[1], company_id = company_id)

  res <- jsonlite::fromJSON(res)

  #Just need the content of the returned json
  res <- res$content

  res

}

