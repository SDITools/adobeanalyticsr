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
#' @export
#'
get_users <- function(company_id = Sys.getenv("AW_COMPANY_ID"),
                      limit = 10,
                      page = 0)
{
  query_params <- list(
    limit = limit,
    page = page
  )

  urlstructure <- paste("users", format_URL_parameters(query_params), sep = "?")

  res <- aw_call_api(req_path = urlstructure, company_id = company_id)
  res <- jsonlite::fromJSON(res)

  res$content
}

