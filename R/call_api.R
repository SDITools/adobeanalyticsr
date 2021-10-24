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
                        client_secret = Sys.getenv("AW_CLIENT_SECRET")) {

    assertthat::assert_that(
        assertthat::is.string(req_path),
        assertthat::is.string(company_id),
        assertthat::is.string(client_id),
        assertthat::is.string(client_secret)
    )

    request_url <- sprintf("https://analytics.adobe.io/api/%s/%s",
                           company_id, req_path)
    token_config <- get_token_config(client_id = client_id, client_secret = client_secret)
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
                           `x-api-key` = client_id,
                           `x-proxy-global-company-id` = company_id
                       ))

    httr::stop_for_status(req)

    httr::content(req, as = "text",encoding = "UTF-8")
}


#' Get token configuration for GET
#'
#' Returns a configuration for `httr::GET` for the correct token type.
#'
#' @param client_id Client ID
#' @param client_secret Client secret
#'
#' @return Config objects that can be passed to `httr::GET` or similar
#' functions (e.g. `httr::RETRY`)
get_token_config <- function(client_id,
                             client_secret) {
    token <- retrieve_aw_token(token_type(.adobeanalytics$token),
                               client_id,
                               client_secret)
    type <- token_type(token)

    switch(type,
        oauth = httr::config(token = token),
        jwt = httr::add_headers(Authorization = paste("Bearer", content(token)$access_token)),
        stop("Unknown token type")
    )
}
