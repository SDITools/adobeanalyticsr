#' Pull a list of projects
#'
#' A list of projects in the account
#'
#' @param includetype Include additional projects not owned by user. The "all" option takes precedence over "shared". If neither guided, or project is included, both types are returned
#' @param expansion Comma-delimited list of additional project metadata fields to include on response. Available values : reportSuiteName, shares, tags, accessLevel, modified, externalReferences, definition
#' @param filterByIds Filter list to only include projects in the specified list (comma-delimited list of IDs)
#' @param locale Locale Default value : en_US
#' @param pagination Return paginated results Available values : true, false (Default)
#' @param ownerId Filter list to only include projects owned by the specified loginId
#' @param limit Number of results per page. Default value: 10
#' @param page Page number (base 0 - first page is "0")
#' @param debug Set to `TRUE` to publish the full JSON request(s) being sent to the API to the console when the
#' function is called. The default is `FALSE`.
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#'
#' @param return A data frame
#' @export
#'
aw_get_projects <- function(includeType = 'all',
                            expansion = NULL,
                            filterByIds = NULL,
                            locale = 'en_US',
                            pagination = FALSE,
                            ownerId = NULL,
                            limit = 10,
                            page = 0,
                            rsids = NULL,
                            segmentFilter = NULL,
                            debug = FALSE,
                            company_id = Sys.getenv("AW_COMPANY_ID")
                            ){
  query_params <- list(
    includeType = includeType,
    expansion = expansion,
    filterByIds = filterByIds,
    locale = locale,
    pagination = pagination,
    ownerId = ownerId,
    limit = limit,
    page = page,
    rsids = rsids,
    segmentFilter = segmentFilter
    )

  urlstructure <- paste('projects', format_URL_parameters(query_params), sep = "?")
  res <- aw_call_api(req_path = urlstructure, debug = debug, company_id = company_id)

  jsonlite::fromJSON(res)
}