#' Get Company Ids
#'
#' This function will quickly pull the list of company ids that you have access to.
#'
#' @param req_path The endpoint for that particular report
#' @param client_id Set in environment args, or pass directly here
#'
#' @return A data frame of company ids and company names
#' @examples
#' \dontrun{
#' get_me()
#' }
#' @export
#' @import assertthat httr
get_me <- function(req_path = 'discovery/me',
                   client_id = Sys.getenv("AW_CLIENT_ID"),
                   client_secret = Sys.getenv("AW_CLIENT_SECRET")) {

    assertthat::assert_that(
        is.string(req_path),
        is.string(client_id),
        is.string(client_secret)
    )

    request_url <- sprintf("https://analytics.adobe.io/%s",
                           req_path)
    token_config <- get_token_config(client_id = client_id, client_secret = client_secret)

    req <- httr::RETRY("GET",
                       url = request_url,
                       encode = "json",
                       body = FALSE,
                       token_config,
                       httr::add_headers(
                           `x-api-key` = client_id
                       ))



    stop_for_status(req)
    res <- httr::content(req, as = "text",encoding = "UTF-8")

    me <- jsonlite::fromJSON(res)

    return(me$imsOrgs$companies %>%
               dplyr::bind_rows() %>%
               dplyr::select(1:2))
}
