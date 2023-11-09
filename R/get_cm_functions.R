#' Get Calculated Metric Functions
#'
#' Returns a full list of calculated metric functions that the user can access.
#'
#' @param id Returns details around a single calculated metric function if you specify the id.
#' You can obtain the desired id by not including an ID value and finding the function in the results.
#' @param debug Set to `TRUE` to publish the full JSON request(s) being sent to the API to the console when the
#' function is called. The default is `FALSE`.
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#'
#' @return Returns a full list of calculated metric functions or a specified function that the user can access.
#'
#' @import dplyr
#' @import assertthat
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#'
get_cm_functions <- function(id = NULL,
                             debug = FALSE,
                             company_id = Sys.getenv("AW_COMPANY_ID")){

  #defined parts of the post request
  if(!is.null(id)) {
    req_path <- glue::glue('calculatedmetrics/functions/{id}')
  }else{
    req_path <- glue::glue('calculatedmetrics/functions')
  }

  req <- aw_call_api(req_path = req_path,
                     company_id = company_id)

  jsonlite::fromJSON(req)
}
