#' Delete A Segment
#'
#' Use this function to delete a specific segment in Adobe Analytics
#'
#' @param id Segment ID to be deleted.
#' @param warn Boolean of whether or not to include a warning message.
#' @param locale language - default 'en_US'
#' @param debug Default `FALSE`. Set this to TRUE to see the information about the api calls as they happen.
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called
#' `AW_REPORTSUITE_ID` exists in `.Renviron` or elsewhere and no `rsid` argument
#' is provided, then the `AW_REPORTSUITE_ID` value will be used. Use [aw_get_reportsuites()]
#' to get a list of available `rsid` values.
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID`
#' exists in `.Renviron` or elsewhere and no `company_id` argument is provided,
#' then the `AW_COMPANY_ID` value will be used. Use [get_me()] to get a list of
#' available `company_id` values.
#'
#' @return A string confirming the segment has or has not been deleted
#'
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom utils menu
#' @export
#'
seg_delete <- function(id = NULL,
                       warn = TRUE,
                       locale = 'en_US',
                       debug = FALSE,
                       rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                       company_id = Sys.getenv("AW_COMPANY_ID")){

  #assert that the 2 key arguments have values
  assertthat::assert_that(assertthat::not_empty(id), msg = 'Argument "id" cannot be empty')
  if(warn){
    if (utils::menu(c("Yes", "No"),
             title= "Are you sure you want to delete this segment") == "1") {

      env_vars <- get_env_vars()
      token_config <- get_token_config(client_id = env_vars$client_id,
                                       client_secret = env_vars$client_secret)

      if (debug) {
        debug_call <- httr::verbose(data_out = TRUE, data_in = TRUE, info = TRUE)
      } else {
        debug_call <- NULL
      }

      req_path <- glue::glue('segments/{id}?locale={locale}')

      request_url <- sprintf("https://analytics.adobe.io/api/%s/%s",
                             company_id, req_path)

      req <- httr::RETRY(verb = 'DELETE',
                         url = request_url,
                         encode = "json",
                         token_config,
                         debug_call,
                         httr::add_headers(
                           `x-api-key` = env_vars$client_id,
                           `x-proxy-global-company-id` = company_id
                         ))

      handle_api_errors(resp = req, body = body)
      # As a fall-through, for errors that fall through handle_api_errors
      httr::stop_for_status(req)
      message(glue::glue('{httr::content(req)$result}: {id} has been deleted'))
    } else { message("Ok, it will not be deleted.")}
  } else {
    env_vars <- get_env_vars()
    token_config <- get_token_config(client_id = env_vars$client_id,
                                     client_secret = env_vars$client_secret)

    if (debug) {
      debug_call <- httr::verbose(data_out = TRUE, data_in = TRUE, info = TRUE)
    } else {
      debug_call <- NULL
    }

    req_path <- glue::glue('segments/{id}?locale={locale}')

    request_url <- sprintf("https://analytics.adobe.io/api/%s/%s",
                           company_id, req_path)

    req <- httr::RETRY(verb = 'DELETE',
                       url = request_url,
                       encode = "json",
                       token_config,
                       debug_call,
                       httr::add_headers(
                         `x-api-key` = env_vars$client_id,
                         `x-proxy-global-company-id` = company_id
                       ))

    handle_api_errors(resp = req, body = body)
    # As a fall-through, for errors that fall through handle_api_errors
    httr::stop_for_status(req)
    message(glue::glue('{httr::content(req)$result}: {id} has been deleted'))
  }

}
