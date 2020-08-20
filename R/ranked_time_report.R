#' Get a timeseries reports
#'
#' @param rsid Adobe report number
#' @param date_range A two length vector of start and end Date objects
#' @param metrics Metric to send
#' @param pages number of report pages
#' @param granularity use either minute, hour, day, week, month, quater, year
#' @param sort either by 'desc' or 'asc' order
#'
#'
#' @export

aa_rankedtime_report <- function(rsid = Sys.getenv('AA_REPORTSUITE_ID'),
                                 date_range,
                                 metrics,
                                 pages = 0,
                                 granularity = 'day',
                                 sort = 'desc'
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
                               page = pages
                             ),
                             statistics = list(
                               functions = c("col-max", "col-min")
                             ) ) )

  res <- aa_get_data("reports/ranked", body = req_body)

  res <- fromJSON(res)

  # Clean up and return as data frame
  res_df <- res$rows %>%
    unnest(data) %>%
    group_by(itemId, value) %>%
    mutate(col= seq_along(value))%>%
    spread(key = col, value = data)

  # Add column names to the dataset based on the metrics and dimensions
  colnames(res_df) <- c('id',granularity,metrics)

  #res_df[2] <- as_date(res_df[2], tz = "UTC", format ="%b %d, %Y")

  df <- data.frame(res_df)

  df
}
