#' Get a ranked report with a breakdown
#'
#' @param date_range A two length vector of start and end Date objects
#' @param metrics Metric to send
#' @param dimensions Dimension to send
#' @param num_rows How many rows
#' @param rsid Adobe report number
#'

#' @export
#' @import assertthat httr tidyverse
#'
aa_breakdown_report <- function(date_range,
                             metrics,
                             dimensions,
                             top = 500,
                             metricSort = 'desc',
                             metricFilters = NULL,
                             dimensionSort = NULL,
                             rsid = Sys.getenv("AA_RSID")) {

  # set the timeframe variable
  timeframe <- make_timeframe(date_range[[1]], date_range[[2]])

  metrics_information <- list(metrics,seq_along(metrics)-1, metricSort)

  meta <- purrr::pmap(metrics_information,addmetrics)

  req_body <- structure(list(rsid = Sys.getenv("AA_RSID"),
                             globalFilters = list(list(
                               type = "dateRange",
                               dateRange = timeframe)),
                             metricContainer = list(
                               metrics = meta
                             ),
                             dimension = sprintf("variables/%s",dimensions[1]),
                             settings = list(
                               countRepeatInstances = TRUE,
                               limit = top,
                               page = 0
                               #dimensionSort = "asc"
                             ),
                             statistics = list(
                               functions = c("col-max", "col-min")
                             ) ) )

  res <- aa_get_data("reports/ranked", body = req_body)

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
  itemids<- res_df$itemId[[1]]

      # set the timeframe variable
      #already set  --> timeframe <- make_timeframe(date_range[[1]], date_range[[2]])

      metrics_information <- list(metrics,seq_along(metrics)-1)
      #new list for building the metricFilter list
      metric_with_filter <- purrr::pmap(metrics_information,addmetricsfilters)

      metric_filters <- list(id = 0, type = 'breakdown', dimension = dimensions[1], itemId = itemids)

      dims <- purrr::pmap(metric_filters, breakdowns)

      req_body <- structure(list(rsid = Sys.getenv("AA_RSID"),
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
                                 ) ) )

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

  # Add column names to the dataset based on the metrics and dimensions
  #colnames(res_df2) <- c('id',dimensions[1],dimensions[2],metrics)
    return(res_df2)

}

