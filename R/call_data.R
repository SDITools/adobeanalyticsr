#' Gets the data from Adobe Analytics API v2 - Internal Function
#'
#' This gives a raw call to the API, but it is intended other functions call this one
#'
#' @param req_path The endpoint for that particular report
#' @param body An R list that will be parsed to JSON
#' @param company_id Set in environment args, or pass directly here
#' @param client_id Set in environment args, or pass directly here
#' @param client_secret Set in environment args, or pass directly here
#' @param debug Set this to TRUE to see the information about the api calls as they happen.
#'
#' @examples
#'
#' \dontrun{
#'
#' aa_call_data("reports/ranked",
#'             body = list(..etc..),
#'             company_id = "blah")
#'
#' }
#'
#' @export
#' @import assertthat httr purrr
#'
aa_call_data <- function(req_path,
                        body = NULL,
                        company_id = Sys.getenv("AA_COMPANY_ID"),
                        client_id = Sys.getenv("AA_CLIENT_ID"),
                        client_secret = Sys.getenv("AA_CLIENT_SECRET"),
                        debug = FALSE){

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
                     verbose(data_out = debug),
                     httr::add_headers(
                       `x-api-key` = client_id,
                       `x-proxy-global-company-id` = company_id
                     ))
  stop_for_status(req)

  if(status_code(req) == 206  & length(content(req)$columns$columnErrors[[1]]) != 0) {
    stop(paste0('Pease check your metrics. You used an ',content(req)$columns$columnErrors[[1]]$errorCode,' - ',content(req)$columns$columnErrors[[1]]$errorDescription))
  } else if(status_code(req) == 206){
    stop(paste0('Please check your dimensions. It did not pull back any data.'))
  } else if(status_code(req) == 200 & content(req)$totalElements == 0) {
    stop("No data was returned while a valid 200 response code was returned. Consider changing 'include_unsecified' to TRUE in your function call.")
  }

  httr::content(req, as = "text",encoding = "UTF-8")

}
