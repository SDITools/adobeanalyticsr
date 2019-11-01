#' Get a timeseries reports
#'
#' @param date_range A two length vector of start and end Date objects
#' @param metrics Metric to send
#' @param top the number of rows to pull
#' @param granularity the breakdown of the date - hour, day, month, quarter, year
#' @param metricFilters the filters to breakdown the metrics by (still in dev)
#' @param sort the order of date either asc or desc (note: is it good that there is no sorting by metrics?)
#' @param rsid Adobe report number
#'
#' @export
aa_timeseries_report <- function(date_range,
                                 metrics,
                                 top = 50,
                                 pages = 0,
                                 granularity = 'day',
                                 metricFilters = NULL,
                                 sort = 'asc',
                                 rsid = Sys.getenv("AA_RSID")){


  # set the timeframe variable
  timeframe <- make_timeframe(date_range[[1]], date_range[[2]])

  meta <- map2(metrics, seq_along(metrics)-1, addtimeseriesmetrics)

  req_body <- structure(list(rsid = Sys.getenv("AA_RSID"),
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
                               limit = top,
                               page = pages
                             ),
                             statistics = list(
                               functions = c("col-max", "col-min")
                             ) ) )

  res <- aa_get_data("reports/ranked", body = req_body)

  res <- fromJSON(res)

  # Clean up and return as data frame
  res_df <- res$rows
  res_df <- as.data.frame(matrix(unlist(res_df), nrow= length(res_df), byrow = T))

  # Add column names to the dataset based on the metrics and dimensions
  colnames(res_df) <- c('id',granularity,metrics)

  res_df
}

