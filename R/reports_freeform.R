#' Get a ranked report
#'
#' Organizes the arguments into a json string and then structures the data after the internal function makes
#' the api call. Only runs a single dimension with as many metrics as you want.
#'
#' @param rsid Adobe report suite id number. Taken from the global environment by default if not provided.
#' @param date_range A two length vector of start and end Date objects
#' @param metrics Metric to send
#' @param dimensions Dimension to send
#' @param top How many rows. Defualt is set to 50
#' @param metricSort Presorts the table by metrics. Values are either 'asc' or 'desc'.
#' @param dimensionSort Presorts the table by dimension.  Not as helpful as metricSort but still has some use.  If metricSort is 'False' the dimension sort will be used.
#'
#' @return Data Frame
#'
#'
#' @import assertthat
#' @import httr
#' @import tidyverse
#' @import jsonlite
#' @import httr
#' @import dplyr
#' @import curl
#' @import tidyverse
#' @import stringr
#'
#' @export
aa_ranked_report <- function(rsid = Sys.getenv("AA_REPORTSUITE_ID"),
                             date_range,
                             metrics,
                             dimensions,
                             top = 50,
                             metricSort = FALSE,
                             dimensionSort = 'asc') {

  # set the timeframe variable
  timeframe <- make_timeframe(date_range[[1]], date_range[[2]])

  metrics_information <- list(metrics,seq_along(metrics)-1, metricSort)

  meta <- purrr::pmap(metrics_information,addmetrics)

  req_body <- structure(list(rsid = rsid,
                             globalFilters = list(list(
                               type = "dateRange",
                               dateRange = timeframe)),
                             metricContainer = list(
                               metrics = meta
                             ),
                             dimension = sprintf("variables/%s",dimensions),
                             settings = list(
                               countRepeatInstances = TRUE,
                               limit = top,
                               page = 0,
                               dimensionSort = "asc"
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
                  group_by(itemId,value) %>%
                  mutate(col = seq_along(data)) %>%
                  spread(key=col, value=data)
  }

  # Add column names to the dataset based on the metrics and dimensions
  colnames(res_df) <- c(paste0(dimensions,'id'),dimensions,metrics)

  res_df
}
