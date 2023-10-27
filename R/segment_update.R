#' Update A Segment
#'
#' Update a
#'
#' @param id The id of the segment you are wanting to update
#' @param updates List of changes or entire JSON definition object.
#' @param locale The URL query parameter locale. Supported values are en_US, fr_FR,
#' ja_JP, de_DE, es_ES, ko_KR, pt_BR, zh_CN, and zh_TW. This argument specifies
#' which language is to be used for localized sections of responses.
#' @param debug Set to `TRUE` to publish the full JSON request(s) being sent to the API to the console when the
#' function is called. The default is `FALSE`.
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#'
#' @return json string of updated segment information
#'
#' @import dplyr
#' @import assertthat
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @export
#'
seg_update <- function(id = NULL,
                       updates = NULL,
                       locale = 'en_US',
                       debug = FALSE,
                       company_id = Sys.getenv("AW_COMPANY_ID")){

  #defined parts of the post request
  if(is.null(id)) {
    stop('The argument "id" value must be provided')
  }
    req_path <- glue::glue('segments/{id}')


    req <- aw_put_data(req_path = req_path,
                       body = updates,
                       debug = debug,
                       company_id = company_id)

  jsonlite::fromJSON(req)
}
