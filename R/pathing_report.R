#' A pathing report is a report showing how often visitors go from
#' page A to page B to page C on the site.
#'
#' @param rsid Report suite id
#' @param date.from Start date for the report (YYYY-MM-DD)
#' @param date.to End date for the report (YYYY-MM-DD)
#' @param metric Single metric to include in the report (usually 'pageviews')
#' @param element Single pathed element (usually 'page')
#' @param pattern Character vector of items in the path (up to 3)
#' use '::anything::' as a wildcard. i.e.
#' c("Home", "::anything::", "::anything::")
#' will return all paths that start with the "home" page.
#' @param top Number of rows to return (defaults to 1000)
#' @param start Row offset
#' @param max_attempts Number of API attempts to get the report before stopping (defaults to 2)
#' You may need to increase this for larger result sets as it may take more
#' attempts to get the report.
#'
#' @examples
#' \dontrun{
#' path_pattern <- c("Home", "::anything::", "::anyting::")
#' report <- pathing_report("YourReportSuiteId",
#'                          "2021-01-01",
#'                          "2021-01-31",
#'                          metric="pageviews",
#'                          element="page",
#'                          pattern=path_pattern)
#' }
#'
#' @export
#'
pathing_report <- function(rsid, date.from, date.to, metric, element, pattern,
                           top = 1000, start = 1, max_attempts = 2) {
  #========Build the JSON report description object to be sent to the API======
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$reportSuiteID <- jsonlite::unbox(rsid)
  report.description$reportDescription$dateFrom <- jsonlite::unbox(date.from)
  report.description$reportDescription$dateTo <- jsonlite::unbox(date.to)
  report.description$reportDescription$metrics <- data.frame(id = metric)
  report.description$reportDescription$elements <- list(list(id = jsonlite::unbox(element),
                                                             top = jsonlite::unbox(top),
                                                             startingWith = jsonlite::unbox(start),
                                                             pattern = as.list(pattern)
  ))

  #=======Send the reportDescription to the API==============
  url <- "https://api.omniture.com/admin/1.4/rest/?method=Report.Queue"
  client_id <- Sys.getenv("AW_CLIENT_ID")
  client_secret <- Sys.getenv("AW_CLIENT_SECRET")

  res <- httr::POST(
    url,
    body = jsonlite::toJSON(report.description),
    encode = "raw",
    get_token_config(client_id, client_secret),
    httr::add_headers(
      `x-api-key` = client_id
    ))

  report_id <- jsonlite::fromJSON(httr::content(res, as = "text"))$reportID

  data <- get_report(report_id, max.attempts = max_attempts)

  return(data)
}
