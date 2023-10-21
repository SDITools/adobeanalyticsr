#' Edit a project in Adobe
#'
#' Edits an existing project in Adobe
#'
#' @param projectId The project id to be edited
#' @param body The json string in list format which includes the edits to be made
#' @param expansion Comma-delimited list of additional project metadata fields to include on response. Available values : reportSuiteName, shares, tags, accessLevel, modified, externalReferences, definition
#' @param locale Locale Default value : en_US
#' @param debug Set to `TRUE` to publish the full JSON request(s) being sent to the API to the console when the
#' function is called. The default is `FALSE`.
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#'
#' @return A json string
#' @export
#'
proj_update <- function(projectId = NULL,
                        body = NULL,
                        expansion = NULL,
                        locale = 'en_US',
                        debug = FALSE,
                        company_id = Sys.getenv("AW_COMPANY_ID")
                            ){
  query_params <- list(
    expansion = expansion,
    locale = locale
  )

  req_path <- glue::glue("projects/{projectId}")
  urlstructure <- paste(req_path, format_URL_parameters(query_params), sep = "?")

  res <- aw_put_data(req_path = urlstructure,
                     body = body,
                     content_type = 'application/json',
                     debug = debug,
                     company_id = company_id)

  jsonlite::fromJSON(res)
}