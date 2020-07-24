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
#'


# 1 pull the first dimension ranked by visits and save the top 500 to a list

# 2 pull the next dimension filtered in a map function for each of the 500
# different 'page' ids and save that data in a list

# 3 run the next dimension using the 50000 dimension ids and save that to a list
# using the 5000 different dimensions

#key things to keep in mind as i do this are:
# output must have dimensioon 1 and 2 brokend down accordingly int eh data so we should build a data
# frame of the reuslts and the numbe of filter dimensions with that result
# page 1 + 10
# page 2 + 10
# to do this i need to know th enumber of rows that came back from my request and then build out the resulting
# df that i will then rbind? or cbind to include the rsulting metrics?  The finall call will include all the metrics
# but, really, it isn't hta tbig of a deal to pull one dimension with multiple metrics, what should i do
# to keep that data or prevent it from lockin gup my system beecause i'm only worried about getting th ebreakdown
# numbers for it.  I jus tneed the last row of numbers

#sample numbers and fields just for show
date_range <- c('2020-02-02', '2020-02-05')
metrics <- c('visits', 'visitors')
dimensions <- c( 'page', 'daterangeday', 'mobiledevicetype')
#dimensions <- 'lasttouchchanneldetail'
top <- 200

aa_breakdown_report <- function(date_range,
                             metrics,
                             dimensions = dimensions,
                             top = 500,
                             metricSort = 'desc',
                             dimensionSort = NULL,
                             rsid = Sys.getenv("AA_REPORTSUITE_ID")) {

#setup top sort if more than one is defined.
 # error handling for more than one dimension listed and not an equal number 'top' items are listed
 # of dimensions more than one is listed.

  #first call for first dimensions
  function(date_range = date_range,
           dim = dimensions[1],
           metrics = metrics,
           top = top[1],
           metricSort = metricSort,
           dimensionSort = dimensionSort,
           rsid = rsid)

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

  ids_1 <- res$rows$itemId

#start the next api pull for dimension 2
dims2list <- list(idlist = ids_1,
                  metrics = metrics,
                  dim = dimensions[2],
                  id2 = seq_along(metrics)-1)


req_body <- function(ids_1) {
                list(rsid = rsid,
                           globalFilters = list(list(
                             type = "dateRange",
                             dateRange = timeframe)),
                           metricContainer = list(
                             metrics = list(list(
                               columnId = id2[1],
                               id =  paste0("metrics/",metrics[1]),
                               filters = list(
                                 id2[1]
                               ))
                               ,
                               list(
                               columnId = id2[2],
                               id =  paste0("metrics/",metrics[2]),
                               filters = list(
                                 id2[2]
                               )
                              )
                            ),
                           metricFilters = list(
                             list(
                               id = id2[1],
                               type = "breakdown",
                               dimension =  paste0("variables/", dimensions[1]),
                               itemId = ids_1
                               ),
                             list(
                               id = id2[2],
                               type = "breakdown",
                               dimension =  paste0("variables/", dimensions[1]),
                               itemId = ids_1
                             )
                            )
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
                           )
                )
               }


req_body_2 <- map(ids_1, req_body)

jsstring <- map(req_body_2, structure)



apicall <-  function(calls = jsstring){
  aa_get_data("reports/ranked", body = calls)
}

res <- map(jsstring, apicall)

result <- map(res, fromJSON)


rows <- lapply(result, `[`, 'rows')

rows2 <- head(rbind.fill(rows))
rows$rows.value
str(rows)
result[1:70][[1]]$rows
res <- fromJSON(res)

# Clean up and return as data frame
res_df <- res$rows