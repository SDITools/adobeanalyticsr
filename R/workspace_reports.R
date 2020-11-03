#' Use a prebuilt json query to pull a ranked report
#'
#' Organizes the arguments into a json string and then structures the data after the internal function makes
#' the api call. Only runs a single dimension with as many metrics as you want.
#'
#' @param company_id Company Id.  Taken from the global environment by default if not provided.
#' @param req_body The json string copied from Workspace
#' @param debug default is FALSE  but set to TRUE to see the json request being sent to the Adobe API
#'
#'
#' @export
#'
aa_workspace_report <- function(req_body = sample,
                                company_id = Sys.getenv('AA_COMPANY_ID'),
                                debug = FALSE) {
  req_body <- jsonlite::fromJSON(req_body)
  #build the necessary naming items for the result
    #build the metric column names
    metrics <-  gsub(".*/", "", req_body$metricContainer$metrics$id)
    #build the dimension column names
    dimensions <- gsub(".*/", "", req_body$dimension)
  #make the request
  if(debug == FALSE) {
    res <- aa_call_data("reports/ranked", body = req_body, company_id = company_id)
  }
  if(debug == TRUE) {
    res <- aa_call_data_debug("reports/ranked", body = req_body, company_id = company_id)
  }
  #reformat from JSON
  res <- jsonlite::fromJSON(res)
  # Clean up and return only data rows
  res_df <- res$rows
  # If more than one metric the value list needs to be spread to individual columns
  if(length(metrics) > 1 ) {
     res_df <- res_df %>%
                  tidyr::unnest(data) %>%
                  dplyr::group_by(itemId,value) %>%
                  dplyr::mutate(col = seq_along(data)) %>%
                  tidyr::spread(key=col, value=data)
     }

  # Add column names to the dataset based on the metrics and dimensions
  colnames(res_df) <- c(paste0(dimensions,'id'),dimensions,metrics)
  #return it as a datafram
  df <- data.frame(res_df)
  return(df)

}
