#' Gets the data from Adobe Analytics API v2 - Internal Function
#'
#' This gives a raw call to the API, but it is intended other functions call this one
#'
#' @noRd
#'
#' @param req_path The endpoint for that particular report
#' @param debug Default `FALSE`. Set this to TRUE to see the information about the api calls as they happen.
#' @param body An R list that will be parsed to JSON
#' @param company_id Set in environment args, or pass directly here
#' @param use_oob Always set to TRUE. Needed for tests
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
                         debug = FALSE,
                         company_id,
                         use_oob = TRUE
){
    assertthat::assert_that(
        is.string(req_path),
        assertthat::not_empty(body),
        is.string(company_id)
    )

    env_vars <- get_env_vars()
    token_config <- get_token_config(client_id = env_vars$client_id,
                                     client_secret = env_vars$client_secret)

    request_url <- sprintf("https://analytics.adobe.io/api/%s/%s",
                           company_id, req_path)

    debug_call <- NULL

    if (debug) {
        debug_call <- httr::verbose(data_out = TRUE, data_in = TRUE, info = TRUE)
    }

    req <- httr::RETRY("POST",
                       url = request_url,
                       body = body,
                       encode = "json",
                       token_config,
                       debug_call,
                       httr::add_headers(
                           `x-api-key` = env_vars$client_id,
                           `x-proxy-global-company-id` = company_id
                       ))

    stop_for_status(req)

    req_errors <- content(req)$columns$columnErrors[[1]]

    if(status_code(req) == 206  & length(req_errors) != 0) {
        stop(paste0('The error code is ', req_errors$errorCode, ' - ', req_errors$errorDescription))
    } else if(status_code(req) == 206) {
        stop(paste0('Please check the metrics your requested. A 206 error was returned.'))
    }

    httr::content(req, as = "text",encoding = "UTF-8")
}
