#' Validate a segment in Adobe Analytics
#'
#' Returns a segment validation for a segment contained in a json string object.
#'
#' @param segment_body The json string of the segment that is being validated (required)
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used. Use [aw_get_reportsuites()] to get a list of available `rsid` values.
#' @param debug This enables the api call information to show in the console for help with debugging issues. default is FALSE
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#' @param client_id Set in environment args, or pass directly here
#' @param client_secret Set in environment args, or pass directly here
#'
#' @return A validation True or False response
#'
#' @import dplyr
#' @import assertthat
#' @importFrom glue glue
#' @export
#'
seg_val <- function(segment_body = NULL,
                    rsid = Sys.getenv('AW_REPORTSUITE_ID'),
                    debug = FALSE,
                    company_id = Sys.getenv("AW_COMPANY_ID"),
                    client_id = Sys.getenv("AW_CLIENT_ID"),
                    client_secret = Sys.getenv("AW_CLIENT_SECRET")){
  #validate arguments
  if(is.null(segment_body)) {
    stop('The arguments `segment_body` must be included.')
  }

  #defined parts of the post request
  req_path <- glue::glue('segments/validate?rsid={rsid}')

  req <- aw_post_attr(req_path = req_path,
                      body = segment_body,
                      rsid = rsid,
                      company_id = company_id,
                      client_id = client_id,
                      client_secret = client_secret)

  if (jsonlite::fromJSON(req)$valid) {
    "The segment is valid."
  } else {
    purrr::map_df(jsonlite::fromJSON(req)$errors, jsonlite::fromJSON) %>%
      dplyr::relocate(3, 1)
  }
}
