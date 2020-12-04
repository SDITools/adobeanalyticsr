#' Gets the data from Adobe Analytics API v2
#'
#' This gives a raw call to the API, but it is intended other functions call this one
#'
#' @noRd
#'
#' @param req_path The endpoint for that particular report
#' @param debug Default `FALSE`. Set this to TRUE to see the information about the api calls as they happen.
#' @param company_id Set in environment args, or pass directly here
#' @param client_id Set in environment args, or pass directly here
#' @param client_secret Set in environment args, or pass directly here
#' @param use_oob Always set to TRUE. Needed for tests
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
                        debug = FALSE,
                        company_id = Sys.getenv("AW_COMPANY_ID"),
                        client_id = Sys.getenv("AW_CLIENT_ID"),
                        client_secret = Sys.getenv("AW_CLIENT_SECRET"),
                        use_oob = TRUE ){

  assertthat::assert_that(
    assertthat::is.string(req_path),
    assertthat::is.string(company_id),
    assertthat::is.string(client_id),
    assertthat::is.string(client_secret)
  )

  # creates token to aa.oauth if not present
  token <- aw_token(client_id, client_secret, use_oob = use_oob)

  request_url <- sprintf("https://analytics.adobe.io/api/%s/%s",
                         company_id, req_path)

  if(debug == F) {
  req <- httr::RETRY("GET",
                     url = request_url,
                     encode = "json",
                     body = FALSE,
                     httr::config(token = token),
                     httr::add_headers(
                       `x-api-key` = client_id,
                       `x-proxy-global-company-id` = company_id
                     ))
  }
  if(debug == T) {
    req <- httr::RETRY("GET",
                       url = request_url,
                       encode = "json",
                       body = FALSE,
                       httr::config(token = token),
                       httr::verbose(data_out = TRUE, data_in = TRUE, info = TRUE),
                       httr::add_headers(
                         `x-api-key` = client_id,
                         `x-proxy-global-company-id` = company_id
                       ))
  }
  httr::stop_for_status(req)

  httr::content(req, as = "text",encoding = "UTF-8")
}
