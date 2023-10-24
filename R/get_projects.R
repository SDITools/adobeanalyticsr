#' Pull a list of projects
#'
#' A list of projects in the account
#'
#' @param includeType Include additional projects not owned by user. The "all" option takes precedence over "shared".
#' If neither guided, or project is included, both types are returned
#' @param expansion Comma-delimited list of additional project metadata fields to include on response.
#' Available values : reportSuiteName, ownerFullName, modified, tags, accessLevel, externalReferences, definition
#' @param locale Locale Default value : en_US
#' @param limit Number of results per page. Default value: 1000
#' @param page Page number (base 0 - first page is "0")
#' @param debug Set to `TRUE` to publish the full JSON request(s) being sent to the API to the console when the
#' function is called. The default is `FALSE`.
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#'
#' @return A data frame
#' @export
#'
aw_get_projects <- function(includeType = 'all',
                            expansion = NULL,
                            locale = 'en_US',
                            limit = 1000,
                            page = 0,
                            debug = FALSE,
                            company_id = Sys.getenv("AW_COMPANY_ID")
                            ){
  query_params <- list(
    includeType = includeType,
    expansion = expansion,
    locale = locale,
    limit = limit,
    page = page
    )

  urlstructure <- paste('projects', format_URL_parameters(query_params), sep = "?")

  res <- aw_call_api(req_path = urlstructure,
                     debug = debug,
                     company_id = company_id)

  jsonlite::fromJSON(res)
}
