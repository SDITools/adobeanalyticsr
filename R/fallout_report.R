#' fallout_report
#'
#' A fallout report shows how visitors drop out as part of a specified path
#'
#' @param rsid report suite id
#' @param date.from Start date for the report (YYYY-MM-DD)
#' @param date.to End date for the report (YYYY-MM-DD)
#' @param metrics List of metrics to include in the report
#' @param element Single pathed element (usually 'page')
#' @param checkpoints Character vector of checkpoints in the fallout path
#' @param max_attempts Number of API attempts to get the report before stopping (defaults to 2)
#' You may need to increase this for larger result sets as it may take more
#' attempts to get the report.
#'
#' @examples
#' \dontrun{
#' df <- fallout_report("reportSuiteId", "2021-08-01", "2021-08-02",
#'                      metric = "pageviews",
#'                      element = "page",
#'                      checkpoints = c("Home", "Page 2"))
#' }
#'
#' @export
fallout_report <- function(rsid,
                           date.from,
                           date.to,
                           metrics,
                           element,
                           checkpoints,
                           max_attempts = 3) {

  # Build the JSON report description
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$dateFrom <- jsonlite::unbox(date.from)
  report.description$reportDescription$dateTo <- jsonlite::unbox(date.to)
  report.description$reportDescription$reportSuiteID <- jsonlite::unbox(rsid)

  report.description$reportDescription$metrics = data.frame(id = metrics)
  report.description$reportDescription$elements = list(list(id = jsonlite::unbox(element),
                                                            checkpoints = checkpoints))


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

  if (res$status == 200) {
    report_id <- jsonlite::fromJSON(httr::content(res, as = "text"))$reportID
  } else {
    print("Error generating report")
    print(httr::content(res, 'text'))
  }


  data <- get_report(report_id, max.attempts = max_attempts)

  return(data)
}