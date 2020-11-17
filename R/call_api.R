#' Gets the data from Adobe Analytics API v2
#'
#' This gives a raw call to the API, but it is intended other functions call this one
#'
#' @noRd
#'
#' @param req_path The endpoint for that particular report
#' @param company_id Set in environment args, or pass directly here
#' @param client_id Set in environment args, or pass directly here
#' @param client_secret Set in environment args, or pass directly here
#'
#' @examples
#'
#' \dontrun{
#'
#' aa_call_api("reports/ranked",
#'             company_id = "blah")
#'
#' }
#'
#' @import assertthat httr
aw_call_api <- function(req_path,
                        company_id = Sys.getenv("AW_COMPANY_ID"),
                        client_id = Sys.getenv("AW_CLIENT_ID"),
                        client_secret = Sys.getenv("AW_CLIENT_SECRET")){

  assertthat::assert_that(
    is.string(req_path),
    is.string(company_id),
    is.string(client_id),
    is.string(client_secret)
  )

  # creates token to aa.oauth if not present
  token <- aw_token(client_id, client_secret)

  request_url <- sprintf("https://analytics.adobe.io/api/%s/%s",
                         company_id, req_path)

  req <- httr::RETRY("GET",
                     url = request_url,
                     encode = "json",
                     body = FALSE,
                     config(token = token),
                     httr::add_headers(
                       `x-api-key` = client_id,
                       `x-proxy-global-company-id` = company_id
                     ))
  stop_for_status(req)
  httr::content(req, as = "text",encoding = "UTF-8")
}
