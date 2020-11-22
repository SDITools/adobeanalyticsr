#' Get Company Ids
#'
#' This function will quickly pull the list of company ids that you have access to.
#'
#' @param req_path The endpoint for that particular report
#' @param client_id Set in environment args, or pass directly here
#' @param client_secret Set in environment args, or pass directly here
#'
#' @examples
#' \dontrun{
#' get_me()
#' }
#'
#' @export
#' @import assertthat httr
get_me <- function(req_path = 'discovery/me',
                    client_id = Sys.getenv("AW_CLIENT_ID"),
                    client_secret = Sys.getenv("AW_CLIENT_SECRET")){

  assertthat::assert_that(
    is.string(req_path),
    is.string(client_id),
    is.string(client_secret)
  )

  # creates token to aa.oauth if not present
  token <- aw_token(client_id, client_secret)

  request_url <- sprintf("https://analytics.adobe.io/%s",
                          req_path)

  req <- httr::RETRY("GET",
                     url = request_url,
                     encode = "json",
                     body = FALSE,
                     config(token = token),
                     httr::add_headers(
                       `x-api-key` = client_id
                     ))
  stop_for_status(req)
  res <- httr::content(req, as = "text",encoding = "UTF-8")

  me <- jsonlite::fromJSON(res)

  message('Your data is now available!')

  return(me$imsOrgs$companies %>%
           dplyr::bind_rows() %>%
           dplyr::select(1:2))
}
