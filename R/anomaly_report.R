#' Anomaly Report
#'
#' Get an anomaly report for one or more metrics
#'
#' @param rsid Adobe report number
#' @param date_range A two length vector of start and end Date objects (default set to show last 30 days)
#' @param metrics Metric to send
#' @param pages number of report pages
#' @param granularity use either minute, hour, day, week, month, quarter, year
#' @param sort either by 'desc' or 'asc' order
#' @param anomalyDetection logical statement for including anomaly. Default is TRUE
#'
#'
#' @export

aa_anomaly_report <- function(company_id = Sys.getenv('AA_COMPANY_ID'),
                                 rsid = Sys.getenv('AA_REPORTSUITE_ID'),
                                 date_range = c(Sys.Date()-31, Sys.Date()-1),
                                 metrics,
                                 pages = 0,
                                 granularity = 'day',
                                 sort = 'asc',
                                 anomalyDetection = TRUE
                                 ){


  # set the timeframe variable
  timeframe <- make_timeframe(date_range[[1]], date_range[[2]])

  if(granularity == "hour") {
    limit <- as.numeric(as.Date(date_range[[2]]) - as.Date(date_range[[1]]))*24
  } else {
    limit <- as.numeric(as.Date(date_range[[2]]) - as.Date(date_range[[1]]))
  }
  if(pages > 0) {
    limit <- limit/pages
  }

  meta <- map2(metrics, seq_along(metrics)-1, addtimeseriesmetrics)

  req_body <- structure(list(rsid = rsid,
                             globalFilters = list(list(
                               type = "dateRange",
                               dateRange = timeframe)),
                             metricContainer = list(
                               metrics = meta,
                               metricFilters = list(
                                list(id = '0',
                                    type = "dateRange",
                                    dateRange = timeframe)
                             )),
                             dimension = sprintf("variables/daterange%s",granularity),
                             settings = list(
                               dimensionSort = sort,
                               limit = limit,
                               page = pages,
                               nonesBehavior = "return-nones",
                               includeAnomalyDetection = anomalyDetection
                             ) ) )

  res <- aa_call_data("reports/ranked", body = req_body,  company_id = company_id)

  res <- fromJSON(res)

  # Clean up and return as data frame
  if(length(metrics) > 1) {
    columnames <- colnames(res$rows[3:7])
    res_df <- res$rows %>%
      unnest(all_of(columnames)) %>%
      group_by(itemId, value) %>%
      mutate(metric = metrics) %>%
      relocate(metric, .after = value) %>%
      rename(date = value) %>%
      mutate(date = as.Date(date, format = '%b %d, %Y'))
  } else {
    res_df <- res$rows %>%
      mutate(metric = metrics) %>%
      relocate(metric, .after = value) %>%
      rename(date = value) %>%
      mutate(date = as.Date(date, format = '%b %d, %Y'))
  }

  # Add column names to the dataset based on the metrics and dimensions
  res_df
  colnames(res_df) <- c('id',granularity,metrics)

  #res_df[2] <- as_date(res_df[2], tz = "UTC", format ="%b %d, %Y")

  df <- data.frame(res_df)

  df
}

