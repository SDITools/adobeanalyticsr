#' Update Calculated Metric Functions
#'
#' Returns a full list of calculated metric functions that the user can access.
#'
#' @param id Returns details around a single calculated metric function if you specify the id.
#' You can obtain the desired id by not including an ID value and finding the function in the results.
#' @param updates List of changes or entire JSON definition object.
#' @param locale All calculated metrics endpoints support the URL query parameter locale.
#' Supported values are en_US, fr_FR, ja_JP, de_DE, es_ES, ko_KR, pt_BR, zh_CN, and zh_TW.
#' This argument specifies which language is to be used for localized sections of responses.
#' @param debug Set to `TRUE` to publish the full JSON request(s) being sent to the API to the console when the
#' function is called. The default is `FALSE`.
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#'
#' @return Returns a full list of calculated metric functions or a speicfied function that the user can access.
#'
#' @import dplyr
#' @import assertthat
#' @import jsonlite
#' @importFrom glue glue
#' @export
#'
cm_update <- function(id = NULL,
                      updates = NULL,
                      locale = 'en_US',
                      debug = FALSE,
                      company_id = Sys.getenv("AW_COMPANY_ID")){

  ## need to update the aw_call_api function to include the argument "update" that
  # will conditionally change the verb to PUT vs GET or POST
  aw_put_api <- function(req_path,
                         body = NULL,
                         update = FALSE,
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

    req <- httr::RETRY(verb = dplyr::case_when(is.null(body) ~ "GET",
                                               update ~ "PUT",
                                               TRUE ~ "POST"),
                       url = request_url,
                       encode = "json",
                       body = body %||% FALSE,
                       token_config,
                       debug_call,
                       httr::add_headers(
                         `x-api-key` = env_vars$client_id,
                         `x-proxy-global-company-id` = company_id
                       ))

    handle_api_errors(resp = req, body = body)
    # As a fall-through, for errors that fall through handle_api_errors
    httr::stop_for_status(req)

    httr::content(req, as = "text", encoding = "UTF-8")
  }
  #defined parts of the post request
  if(is.null(id)) {
    stop('The argument "id" value must be provided')
  }
    req_path <- glue::glue('calculatedmetrics/{id}')


  req <- aw_put_api(req_path = req_path,
                    body = updates,
                    debug = debug,
                    update = TRUE,
                    company_id = company_id)

  jsonlite::fromJSON(req)
}
