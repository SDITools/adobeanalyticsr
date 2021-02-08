#' Get list of users
#'
#' Retrieves a list of all users for the company designated by the auth token.
#'
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use \code{\link{get_me}} to get a list of available `company_id` values.
#' @param limit The number of results to return per page. This argument works in conjunction with the
#' `page` argument. The default is 10.
#' @param page The "page" of results to display. This works in conjunction with the `limit` argument and is
#' zero-based. For instance, if `limit = 20` and `page = 1`, the results returned would be 21 through 40.
#'
#' @return A data frame of users and their meta data.
#' @examples
#' \dontrun{
#' get_users(limit = 10, page = 0)
#' }
#'
#' @import stringr
#' @export
#'
get_users <- function(company_id = Sys.getenv("AW_COMPANY_ID"),
                      limit = 10,
                      page = 0)
{

  vars <- tibble::tibble(limit, page)
  #Turn the list into a string to create the query
  prequery <- list(vars %>% dplyr::select_if(~ !any(is.na(.))))
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

