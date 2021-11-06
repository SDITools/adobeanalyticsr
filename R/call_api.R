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
                        company_id) {

    assertthat::assert_that(
        assertthat::is.string(req_path),
        assertthat::is.string(company_id)
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

    req <- httr::RETRY("GET",
                       url = request_url,
                       encode = "json",
                       body = FALSE,
                       token_config,
                       debug_call,
                       httr::add_headers(
                           `x-api-key` = env_vars$client_id,
                           `x-proxy-global-company-id` = company_id
                       ))

    httr::stop_for_status(req)

    httr::content(req, as = "text",encoding = "UTF-8")
}

