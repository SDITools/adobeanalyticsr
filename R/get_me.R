#' Get all the company ids you are authorized to pull data from using the API
#'
#'
#' @param req_path The endpoint for that particular report
#' @param client_id Set in environment args, or pass directly here
#' @param client_secret Set in environment args, or pass directly here
#'
#' @examples
#'
#' \dontrun{
#'
#' get_me()
#'
#' }
#'
#' @export
#' @import assertthat httr
get_me <- function(req_path = 'discovery/me',
                    client_id = Sys.getenv("AA_CLIENT_ID"),
                    client_secret = Sys.getenv("AA_CLIENT_SECRET")){

  assert_that(
    is.string(req_path),
    is.string(client_id),
    is.string(client_secret)
  )

  # creates token to aa.oauth if not present
  token <- aa_token(client_id, client_secret)

  request_url <- sprintf("https://analytics.adobe.io/%s",
                          req_path)

  req <- httr::RETRY("GET",
                     url = request_url,
                     encode = "json",
                     body = FALSE,
                     config(token = token),
                     verbose(),
                     httr::add_headers(
                       `x-api-key` = client_id
                     ))
  stop_for_status(req)
  res <- httr::content(req, as = "text",encoding = "UTF-8")

  me <- fromJSON(res)


  me$imsOrgs$companies %>% bind_rows()
}
