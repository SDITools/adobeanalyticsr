#' Anomaly Report
#'
#' Get an anomaly report for one or more metrics
#'
#' @param company_id Company Id.  Taken from the global environment by default if not provided.
#' @param rsid Adobe report number
#' @param date_range A two length vector of start and end Date objects (default set to show last 30 days)
#' @param metrics Metric to send
#' @param pages number of report pages
#' @param segmentId use segments to globally filter the results. Use 1 or many.
#' @param granularity use either minute, hour, day, week, month, quarter, year
#' @param sort either by 'desc' or 'asc' order
#' @param anomalyDetection logical statement for including anomaly. Default is TRUE
#'
#'
#' @export

aw_anomaly_report <- function(company_id = Sys.getenv('AW_COMPANY_ID'),
                                 rsid = Sys.getenv('AW_REPORTSUITE_ID'),
                                 date_range = c(Sys.Date()-31, Sys.Date()-1),
                                 metrics,
                                 pages = 0,
                                 granularity = 'day',
                                 sort = 'asc',
                                 segmentId = NA,
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

  #segment filter builder function (segments)
  seg <- function(segmentId) {
    structure(list(type = "segment",
                   segmentId = segmentId))
  }

  segments <- purrr::map(segmentId, seg)

  #create the DateRange list item (dr)
  dr <- list(list(
    type = "dateRange",
    dateRange = timeframe))

  #join Segment and DateRange builder function
  s_dr <- function() {
    if(is.na(segmentId[[1]])) {
      list(list(
        type = "dateRange",
        dateRange = timeframe
      ))
    } else {
      append(segments, dr)
    }
  }

  #Create the global filters (gf)
  gf <- s_dr()

  meta <- map2(metrics, seq_along(metrics)-1, addtimeseriesmetrics)

  req_body <- structure(list(rsid = rsid,
                             globalFilters =
                               gf,
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

  res <- aw_call_data("reports/ranked", body = req_body,  company_id = company_id)

  res <- fromJSON(res)

  # Clean up and return as data frame
  if(length(metrics) > 1) {
    columnames <- colnames(res$rows[3:7])
    res_df <- res$rows %>%
      replace_na(list(0)) %>%
      unnest(columnames) %>%
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

  df <- res_df

  df
}

