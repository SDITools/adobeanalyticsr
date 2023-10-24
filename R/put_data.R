#' Puts data into Adobe Analytics API v2 - Internal Function
#'
#' This gives a raw call to the API, but it is intended other functions call this one
#'
#' @noRd
#'
#' @param req_path The endpoint for that particular report
#' @param body An R list that will be parsed to JSON
#' @param content_type The cotent type of the PUT request body
#' @param debug Default `FALSE`. Set this to TRUE to see the information about the api calls as they happen.
#' @param company_id Set in environment args, or pass directly here
#' @param use_oob Always set to TRUE. Needed for tests
#'
#' @examples
#'
#' \dontrun{
#'
#' aw_put_data("reports/ranked",
#'             body = list(..etc..),
#'             company_id = "blah")
#'
#' }
#' @import assertthat httr purrr
#'
aw_put_data <- function(req_path,
                        body = NULL,
                        content_type = 'application/json',
                        debug = FALSE,
                        company_id,
                        use_oob = TRUE
){
    assert_that(
        is.string(req_path),
        is.list(body),
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

    req <- httr::RETRY("PUT",
                       url = request_url,
                       body = body,
                       encode = "json",
                       token_config,
                       `Content-Type` = content_type,
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
