#' Gets the data from Adobe Analytics API v2 - Internal Function
#'
#' This gives a raw call to the API, but it is intended other functions call this one
#'
#' @noRd
#'
#' @param req_path The endpoint for that particular report
#' @param body An R list that will be parsed to JSON
#' @param company_id Set in environment args, or pass directly here
#' @param client_id Set in environment args, or pass directly here
#' @param client_secret Set in environment args, or pass directly here
#'
#' @examples
#'
#' \dontrun{
#'
#' aw_call_data("reports/ranked",
#'             body = list(..etc..),
#'             company_id = "blah")
#'
#' }
#' @import assertthat httr purrr
#'
aw_call_data <- function(req_path,
                        body = NULL,
                        company_id = Sys.getenv("AW_COMPANY_ID"),
                        client_id = Sys.getenv("AW_CLIENT_ID"),
                        client_secret = Sys.getenv("AW_CLIENT_SECRET")
                        ){

  assert_that(
    is.string(req_path),
    is.list(body),
    is.string(company_id),
    is.string(client_id),
    is.string(client_secret)
  )

  # creates token to aa.oauth if not present
  token <- aw_token(client_id, client_secret)

  request_url <- sprintf("https://analytics.adobe.io/api/%s/%s",
                         company_id, req_path)

  req <- httr::RETRY("POST",
                     url = request_url,
                     body = body,
                     encode = "json",
                     config(token = token),
                     httr::add_headers(
                       `x-api-key` = client_id,
                       `x-proxy-global-company-id` = company_id
                     ))
  stop_for_status(req)

  if(status_code(req) == 206  & length(content(req)$columns$columnErrors[[1]]) != 0) {
    stop(paste0('The error code is ',content(req)$columns$columnErrors[[1]]$errorCode,' - ',content(req)$columns$columnErrors[[1]]$errorDescription))
  } else if(status_code(req) == 206) {
    stop(paste0('Please check the metrics your requested. A 206 error was returned.'))
  }
  httr::content(req, as = "text",encoding = "UTF-8")
}
