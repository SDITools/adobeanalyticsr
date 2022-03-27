#' Gets the data from Adobe Analytics API v2
#'
#' This gives a raw call to the API, but it is intended other functions call this one
#'
#' @noRd
#'
#' @param req_path The endpoint for that particular report
#' @param body Optional, list data structure to use as the body of the request
#' @param debug Default `FALSE`. Set this to TRUE to see the information about the api calls as they happen.
#' @param company_id Set in environment args, or pass directly here
#'
#' @examples
#'
#' \dontrun{
#'
#' aa_call_api(req_path = "reports/ranked",
#'             company_id = "mycompanyid")
#'
#' }
#'
aw_call_api <- function(req_path,
                        body = NULL,
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

    if (debug) {
      debug_call <- httr::verbose(data_out = TRUE, data_in = TRUE, info = TRUE)
    } else {
      debug_call <- NULL
    }

    req <- httr::RETRY(verb = ifelse(is.null(body), "GET", "POST"),
                       url = request_url,
                       encode = "json",
                       body = body %||% FALSE,
                       token_config,
                       debug_call,
                       httr::add_headers(
                           `x-api-key` = env_vars$client_id,
                           `x-proxy-global-company-id` = company_id
                       ))

    httr::stop_for_status(req)


    # From api_call_data -- does this work generally? 
    # req_errors <- httr::content(req)$columns$columnErrors[[1]]

    # if (httr::status_code(req) == 206  & length(req_errors) != 0) {
    #     stop(paste0('The error code is ', req_errors$errorCode, ' - ', req_errors$errorDescription))
    # } else if (status_code(req) == 206) {
    #     stop('Please check the metrics your requested. A 206 error was returned.')
    # }

    httr::content(req, as = "text", encoding = "UTF-8")
}

