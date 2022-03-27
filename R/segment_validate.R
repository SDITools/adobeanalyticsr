#' Validate a segment in adobe analytics
#'
#' Returns a segment validation response for a segment contained in a json string object.
#'
#' @param segment_body The json string of the segment that is being validated (required)
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used. Use [aw_get_reportsuites()] to get a list of available `rsid` values.
#' @param debug This enables the api call information to show in the console for help with debugging issues. default is FALSE
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#'
#' @return If the segment is valid a message saying the segment validates is returned.
#' If the segment doesn't validate the errors are returned in a data frame.
#'
#' @import dplyr
#' @import assertthat
#' @importFrom glue glue
#' @export
#'
seg_val <- function(segment_body = NULL,
                    rsid = Sys.getenv('AW_REPORTSUITE_ID'),
                    debug = FALSE,
                    company_id = Sys.getenv("AW_COMPANY_ID")){
  #validate arguments
  if(is.null(segment_body)) {
    stop('The arguments `segment_body` must be included.')
  }

  #defined parts of the post request
  req_path <- glue::glue('segments/validate?rsid={rsid}')

  req <- aw_call_api(req_path = req_path,
                      body = segment_body,
                      company_id = company_id)

  if (jsonlite::fromJSON(req)$valid) {
    "The segment is valid."
  } else {
    purrr::map_df(jsonlite::fromJSON(req)$errors, jsonlite::fromJSON) %>%
      dplyr::relocate(3, 1)
  }
}
