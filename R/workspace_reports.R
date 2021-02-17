#' Use a prebuilt json query to pull a ranked report
#'
#' Organizes the arguments into a json string and then structures the data after the internal function makes
#' the api call. Only runs a single dimension with as many metrics as you want.
#'
#' @param req_body The json string copied from Workspace
#' @param company_id Company Id.  Taken from the global environment by default if not provided.
#' @param client_id Set in environment args, or pass directly here
#' @param client_secret Set in environment args, or pass directly here
#'
#' @export
#' @import assertthat httr dplyr tidyr
#'
aw_workspace_report <- function(req_body = '',
                                company_id = Sys.getenv('AW_COMPANY_ID'),
                                client_id = Sys.getenv("AW_CLIENT_ID"),
                                client_secret = Sys.getenv("AW_CLIENT_SECRET")) {

  #assert for better error control
  assertthat::assert_that(
    file.exists(req_body),
    is.string(company_id),
    is.string(client_id),
    is.string(client_secret)
  )

  # creates token to aa.oauth if not present
  token <- aw_token(client_id, client_secret)

  #grab the dimensions and metric names from the query
  query <-jsonlite::fromJSON(txt=req_body)

  #build the necessary naming items for the result
  #build the metric column names
  metrics <-  gsub(".*/", "", query$metricContainer$metrics$id)
  #build the dimension column names
  dimensions <- gsub(".*/", "", query$dimension)

  #if metricFilters exist then concatenate the names of the fitler and metric for final metric name
  if(!is.null(query$metricContainer$metricFilters)) {
    #if the the metric filter id is on the second or 30th metric then the filter
    #name needs to be applied on that level and the others need to have '' applied
    mf <- query$metricContainer$metricFilters  %>%
      dplyr::rename('filtername' = 3)
    #unnest the metrics to include multiple filter columns if needed
    mets <- tidyr::unnest(query$metricContainer$metrics, cols = c(columnId, filters))

    metricsinfo <- mets %>%
      dplyr::left_join(mf, by = c('filters' = 'id'))

    finalmnames <- metricsinfo %>%
      dplyr::mutate(id = gsub(".*/", "", id)) %>%
      dplyr::mutate(mfinalname = ifelse(!is.na(filtername), paste0(filtername,'_', id), id) ) %>%
      dplyr::group_by(columnId) %>%
      dplyr::summarise(mfinalname = paste0(mfinalname, collapse = "-")) %>%
      dplyr::pull(mfinalname)

  } else {
    finalmnames <- metrics
  }

  request_url <- sprintf("https://analytics.adobe.io/api/%s/reports/ranked",
                         company_id)

  req <- httr::RETRY("POST",
                     url = request_url,
                     body = upload_file(req_body),
                     encode = "json",
                     config(token = token),
                     httr::add_headers(
                       `x-api-key` = client_id,
                       `x-proxy-global-company-id` = company_id
                     ))

  httr::stop_for_status(req)

  res <- httr::content(req, as = "text",encoding = "UTF-8")

  #reformat from JSON
  res <- jsonlite::fromJSON(res)
  # Clean up and return only data rows
  res_df <- res$rows
  # If more than one metric the value list needs to be spread to individual columns
  res_df <- res_df %>%
    tidyr::unnest(data) %>%
    dplyr::group_by(itemId,value) %>%
    dplyr::mutate(col = seq_along(data)) %>%
    tidyr::spread(key=col, value=data) %>%
    dplyr::ungroup()%>%
    dplyr::select(-itemId)

  # Add column names to the data set based on the metrics and dimensions
  colnames(res_df) <- c(dimensions,finalmnames)

  #return it as a data frame
  df <- data.frame(res_df)

  return(df)
}
