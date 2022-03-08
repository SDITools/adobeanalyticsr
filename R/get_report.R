#' GetReport
#'
#' Internal function polls the Reporting 1.4 API for the given report id and
#' fetches the report data when available.
#'
#' @return Data frame
#'
get_report <- function(report.id,
                       interval.seconds = 10,
                       max.attempts = 3,
                       print.attempts = FALSE,
                       format = "json",
                       page = 0) {

  client_id <- Sys.getenv("AW_CLIENT_ID")
  client_secret <- Sys.getenv("AW_CLIENT_SECRET")

  #======Poll the API until the report is ready========
  get_report_url <- "https://api.omniture.com/admin/1.4/rest/?method=Report.Get"
  request.body <- c()
  request.body$reportID <- jsonlite::unbox(report.id)
  request.body$format <- jsonlite::unbox("json")

  num_tries <- 0
  result <- FALSE
  while (result == FALSE && num_tries < max.attempts) {
    num_tries <- num_tries + 1
    print(paste("Requesting URL attempt #", num_tries, sep = ""))

    response <- httr::POST(get_report_url,
                           body = jsonlite::toJSON(request.body),
                           get_token_config(client_id, client_secret),
                           httr::add_headers(
                             `x-api-key` = client_id
                           ))

    # check the response to see if the data is ready
    if (response$status == 200 || response$status == 400) {
      # Success or BadRequest
      if (response$status == 200) {
        response.content <- list(error = "")
      } else {
        response.content <- jsonlite::fromJSON(httr::content(response, 'text', encoding = 'UTF-8'))
      }
      if (response$status == 400 && response.content$error == 'report_not_ready') {
        result <- FALSE
        Sys.sleep(interval.seconds)
      } else {
        result <- TRUE
      }
    } else {
      print(response$status)
      Sys.sleep(interval.seconds)
    }
  }

  # Check to see if we exited the loop because we have data or we reached
  # the maximum number of retries.
  if (!result || response$status == 400) {
    response.content <- jsonlite::fromJSON(httr::content(response, 'text', encoding = 'UTF-8'))
    if (response.content$error == 'report_not_ready') {
      stop(paste('ERROR: max attempts exceeded for ', get_report_url))
    } else {
      stop(paste('ERROR:', response.content$error, ' -', response.content$error_description))
    }
  }

  #=====Parse the API response and return the report data======================
  data <- jsonlite::fromJSON(httr::content(response,'text', encoding = "UTF-8"))

  if (length(data$report$data) == 0) {
    print("Warning: Your report definition returned an empty data set.")
    return(data.frame())
  }

  report.data <- data$report$data %>% as.data.frame
  return(report.data)

}