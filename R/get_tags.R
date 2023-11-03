#' Get list of tags
#'
#' Retrieve all tag names
#'
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use \code{\link{get_me}} to get a list of available `company_id` values.
#' @param componentId The component id being requested. Default is `NULL`
#' @param componentType The component type being requested. Options include segment, dashboard, bookmark,
#' calculatedMetric, project, dateRange, metric, dimension, virtualReportSuite, scheduledJob, alert, classification.
#' Default is NULL
#' @param tagNames Comma separated vector of tag names.
#' @param limit The number of results to return per page. This argument works in conjunction with the
#' `page` argument. The default is 10.
#' @param page The "page" of results to display. This works in conjunction with the `limit` argument and is
#' zero-based. For instance, if `limit = 20` and `page = 1`, the results returned would be 21 through 40.
#' @param debug Include the output and input of the api call in the console for debugging. Default is FALSE
#'
#'
#' @return A data frame of segments and their meta data.
#'
#' @export
#'
aw_get_tags <- function(company_id = Sys.getenv("AW_COMPANY_ID"),
                        componentId = NULL,
                        componentType = NULL,
                        tagNames = NULL,
                        limit = 1000,
                        page = 0,
                        debug = FALSE)
{

  query_params <- list(
    componentId = componentId,
    componentType = componentType,
    tagNames = tagNames,
    page = page,
    limit = limit)

  if(!is.null(componentId)) {
    baseurl <- 'componentmetadata/tags/search'
  } else if(!is.null(tagNames)) {
    baseurl <- 'componentmetadata/tags/tagnames'
  } else {
    baseurl <- 'componentmetadata/tags'
  }
  urlstructure <- paste(baseurl, format_URL_parameters(query_params), sep = "?")

  res <- aw_call_api(req_path = urlstructure, debug = debug, company_id = company_id)

  if(!is.null(componentId)) {
    jsonlite::fromJSON(res) %>%
      unnest(components)
  } else if(!is.null(tagNames)) {
    jsonlite::fromJSON(res)
  } else {
    jsonlite::fromJSON(res)$content
  }
}




