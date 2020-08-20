#' Gets the data from Adobe Analytics API v2
#'
#' This gives a raw call to the API, but it is intended other functions call this one
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
#' aa_get_data("reports/ranked",
#'             body = list(..etc..),
#'             company_id = "blah")
#'
#' }
#'
#' @export
#' @import assertthat httr purrr
#'
aa_get_data <- function(req_path,
                        body = NULL,
                        company_id = Sys.getenv("AA_COMPANY_ID"),
                        client_id = Sys.getenv("AA_CLIENT_ID"),
                        client_secret = Sys.getenv("AA_CLIENT_SECRET")){

  assert_that(
    is.string(req_path),
    is.list(body),
    is.string(company_id),
    is.string(client_id),
    is.string(client_secret)
  )

  # creates token to aa.oauth if not present
  token <- aa_token(client_id, client_secret)

  request_url <- sprintf("https://analytics.adobe.io/api/%s/%s",
                         company_id, req_path)

  req <- httr::RETRY("POST",
                     url = request_url,
                     body = body,
                     encode = "json",
                     config(token = token),
                     verbose(),
                     httr::add_headers(
                       `x-api-key` = client_id,
                       `x-proxy-global-company-id` = company_id
                     ))
  stop_for_status(req)
  httr::content(req, as = "text",encoding = "UTF-8")

}
