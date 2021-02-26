#' Anomaly Report
#'
#' Get an anomaly report for one or more metrics
#'
#' @param company_id Company Id.  Taken from the global environment by default if not provided.
#' @param rsid Adobe report number
#' @param date_range A two length vector of start and end Date objects (default set to show last 30 days)
#' @param metrics Metric to request the anomaly detection. If multiple metrics, each metric and date will have it's own row.
#' @param segmentId Use segments to globally filter the results. Use 1 or many.
#' @param granularity Use either hour, day (default), week, or month
#' @param quickView Return a list of 3 lists per metric. 1. All Data 2. Data filtered to include only anomalous rows 3. Interactive ggplot line graph
#' @param countRepeatInstances Should the data include repeat instances
#' @param anomalyDetection logical statement for including anomaly. Default is TRUE
#' @param debug default is FALSE but set to TRUE to see the json request being sent to the Adobe API
#'
#' @return If quickView = 'FALSE' (default) then a data frame including the day, metric, data, dataExpected, dataUpperBound, dataLowerBound, and
#' dataAnomalyDetected will be returned.  If quickView = 'TRUE' then a list of three lists will be returned. The first list will be a data frame including
#' all the default columns. The second list item will be a filtered data frame that includes rows where dataAnomalyDetected = 'TRUE'.  The third list
#' item is a visual made using ggplot with the error band and points where the dataAnomalyDetected = 'TRUE'. If more than one metric is in the request
#' and quickView is set to TRUE then the lists will be split by each metric requested.
#'
#' @importFrom scales comma
#' @import ggplot2
#' @export
#'
aw_anomaly_report <- function(company_id = Sys.getenv('AW_COMPANY_ID'),
                                 rsid = Sys.getenv('AW_REPORTSUITE_ID'),
                                 date_range = c(Sys.Date()-31, Sys.Date()-1),
                                 metrics,
                                 granularity = 'day',
                                 segmentId = NA,
                                 quickView = FALSE,
                                 anomalyDetection = TRUE,
                                 countRepeatInstances = TRUE,
                                 debug = FALSE
                                 ){


  # set the timeframe variable
  timeframe <- make_timeframe(date_range[[1]], date_range[[2]])

  if(granularity == "hour") {
    limit <- as.numeric(as.Date(date_range[[2]]) - as.Date(date_range[[1]]))*24
  } else {
    limit <- as.numeric(as.Date(date_range[[2]]) - as.Date(date_range[[1]]))
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
                               countRepeatInstances = countRepeatInstances,
                               limit = limit,
                               page = 0,
                               dimensionSort = 'asc',
                               nonesBehavior = "return-nones",
                               includeAnomalyDetection = anomalyDetection
                             ) ) )

  res <- aw_call_data("reports/ranked", body = req_body,  debug = debug, company_id = company_id)

  res <- fromJSON(res)

  # Clean up and return as data frame
    columnames <- colnames(res$rows[3:7])
    dat <- res$rows %>%
      tidyr::replace_na(list(0)) %>%
      tidyr::unnest(all_of(columnames)) %>%
      dplyr::group_by(itemId, value) %>%
      dplyr::mutate(metric = metrics) %>%
      dplyr::relocate(metric, .after = value) %>%
      dplyr::rename(!!granularity := value) %>%
      dplyr::ungroup() %>%
      select(-itemId)


  # change time variables from character strings
    if("hour" %in% colnames(dat)) {
      dat[names(dat) == 'hour'] <- lubridate::parse_date_time(dat$hour, orders = "HM ymd")
    }
    if("day" %in% colnames(dat)) {
      dat[names(dat) == 'day'] <- as.Date(dat$day, format = '%b %d, %Y')
    }
    if("week" %in% colnames(dat)) {
      dat[names(dat) == 'week'] <- as.Date(dat$week, format = '%b %d, %Y')
    }

    message(paste0('A total of ',nrow(dat), ' rows have been pulled.'))
  if(quickView == F) {
    return(dat)
  }
    if(quickView == T) {
      quickview <- function(metricname) {

        data <- dat %>% dplyr::filter(metric == metricname)

        table <- dat %>% dplyr::filter(metric == metricname & dataAnomalyDetected == T)

        graph <- dat %>%
          dplyr::filter(metric == metricname) %>%
          ggplot2::ggplot(aes_string(x = granularity)) +
          ggplot2::geom_line(aes_string( y = 'data')) +
          ggplot2::geom_point(data = dat %>% dplyr::filter(metric == metricname & dataAnomalyDetected == T),
                              ggplot2::aes_string(y ='data')) +
          ggplot2::geom_ribbon(aes(ymin=dataLowerBound, ymax=dataUpperBound), alpha=0.2) +
          ggplot2::labs(title = metricname,
                        subtitle = paste0('There are ',nrow(dat %>% filter(metric == metricname & dataAnomalyDetected == T)), ' anomalies.'),
                        caption =paste0('There are ',nrow(dat %>% filter(metric == metricname & dataAnomalyDetected == T)), ' anomalies.')) +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none') +
          ggplot2::scale_y_continuous(labels = scales::comma) +
          ggplot2::expand_limits(y=0)

        list(data = data, anoms = table, viz = graph)
        }
        qv <- purrr::map(metrics, quickview)

        return(qv)
      }



}

