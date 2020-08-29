#' Get a ranked report with a breakdown
#'
#' @param date_range A two length vector of start and end Date objects
#' @param metrics Metric to request
#' @param dimensions Dimension to request
#' @param top How many rows
#' @param metricSort Use 'asc' or 'desc' the metrics sort the resulting report
#' @param dimensionSort Leave as NULL (default) if metrics shuld determine the sort of the report
#' @param rsid Adobe report number
#'
#' @export
#' @import assertthat httr tidyverse
#'
aa_breakdown_report <- function(rsid = Sys.getenv("AA_REPORTSUITE_ID"),
                                date_range,
                                metrics,
                                dimensions,
                                top = 10,
                                metricSort = 'desc',
                                dimensionSort = NULL
                             ) {

  # set the timeframe variable
  timeframe <- make_timeframe(date_range[[1]], date_range[[2]])


  if(length(dimensions) != length(top) | length(dimensions) != 1) {
    #make sure top matches length of domain
    stop('Don\'t do it!')
  }


 #get the metrics in a list format
  metrics_information <- list(metrics,seq_along(metrics)-1, metricSort)
 #turn the metrics call to a simple json string
  meta <- purrr::pmap(metrics_information,addmetrics)
  #pull the first dimension
  dim <- dimensions[1]
  #how many items should be pulled?
  top_off <- top[1]

  #create the first call
  req_body <- structure(list(rsid = rsid,
                             globalFilters = list(list(
                               type = "dateRange",
                               dateRange = timeframe)),
                             metricContainer = list(
                               metrics = meta
                             ),
                             dimension = sprintf("variables/%s",dim),
                             settings = list(
                               countRepeatInstances = TRUE,
                               limit = top[1],
                               page = 0
                               #dimensionSort = "asc"
                             ),
                             statistics = list(
                               functions = c("col-max", "col-min")
                             ) ) )
  req_body <- structure(list(rsid = rsid,
                             globalFilters = list(list(
                               type = "dateRange",
                               dateRange = timeframe)),
                             metricContainer = list(
                               metrics = meta
                             ),
                             dimension = sprintf("variables/%s",dim),
                             settings = list(
                               countRepeatInstances = TRUE,
                               limit = top,
                               page = 0,
                               dimensionSort = "asc"
                             ),
                             statistics = list(
                               functions = c("col-max", "col-min")
                             ) ) )
  res <- aa_call_data("reports/ranked", body = req_body)

  res <- fromJSON(res)

  # Clean up and return as data frame
  res_df <- res$rows

  # If more than one metric the value list needs to be spread to individual columns
  if(length(metrics) > 1 ) {
    res_df <- res_df %>%
      unnest(data) %>%
      group_by(itemId, value) %>%
      mutate(col = seq_along(data)) %>%
      spread(key=col, value=data) %>%
      arrange(desc(`1`) )
  }

  # Add column names to the dataset based on the metrics and dimensions
  colnames(res_df) <- c('itemId', dimensions[1],metrics)
#is there more than one dimension?
if(length(dimensions) == 1) {
  return(res_df)
} else {
  itemids<- res_df$itemId

      metrics_information <- list(metrics,seq_along(metrics)-1)
      #new list for building the metricFilter list
      metric_with_filter <- purrr::pmap(metrics_information,addmetricsfilters)

      metric_filters <- list(id = 0, type = 'breakdown', dimension = dimensions[1], itemId = itemids)

      dims <- purrr::pmap(metric_filters, breakdowns)

      req_body <- function(dims) {

        list(rsid = Sys.getenv("AA_RSID"),
             globalFilters = list(list(
               type = "dateRange",
               dateRange = timeframe)),
             metricContainer = list(
               metrics = metric_with_filter,
               metricFilters = dims
             ),
             dimension = sprintf("variables/%s",dimensions[2]),
             settings = list(
               countRepeatInstances = TRUE,
               limit = top,
               page = 0,
               dimensionSort = "asc"
             ),
             statistics = list(
               functions = c("col-max", "col-min")
             ) )
        }

      req_body_2 <- map(dims, req_body)

      jsstring <- map(req_body_2, structure)

      apicall <-  function(calls = jsstring) {
        aa_get_data("reports/ranked", body = calls)
      }

      res <- map(jsstring, apicall)

      res2 <- aa_get_data("reports/ranked", body = req_body)

      res2 <- fromJSON(res2, flatten = TRUE )

      # Clean up and return as data frame
      res_df2 <- res2$rows

      # If more than one metric the value list needs to be spread to individual columns
      if(length(metrics) > 1 ) {
        res_df2 <- res_df2 %>%
          unnest(data) %>%
          group_by(itemId,value) %>%
          mutate(col = seq_along(data)) %>%
          spread(key=col, value=data)
      }
}}
