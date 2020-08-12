#' Use a prebuilt json query to pull a ranked report
#'
#' Organizes the arguments into a json string and then structures the data after the internal function makes
#' the api call. Only runs a single dimension with as many metrics as you want.
#'
#' @param req_body The json string coppied from workspace
#'
#' @return Data Frame
#'
#' @export
#'
#'
aa_workspace_report <- function(req_body) {

  body <- fromJSON(req_body)
  #build the necessary naming items for the result
    #build the metric column names
    metrics <-  gsub(".*/", "", body$metricContainer$metrics$id)
    #build the dimension column names
    dimensions <- gsub(".*/", "", body$dimension)
  #make the request
  res <- aa_get_data("reports/ranked", body = body)
  #reformat from JSON
  res <- fromJSON(res)

  # Clean up and return only data rows
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
  #return it as a datafram
  df <- data.frame(res_df)
  return(df)

}
