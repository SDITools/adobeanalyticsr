#' Check if metrics are custom
#'
#' @param metric Vector of metrics
#'
#' @return Logical, `TRUE` if metric is custom and `FALSE` otherwise
#' @noRd
is_calculated_metric <- function(metric) {
  grepl('cm[1-9]*_*', metric)
}


#' Make a component lookup table
#'
#' @param rsid Reportsuite ID
#' @param company_id Company ID
#' @param calculated_metrics Vector of calculated metric IDs. Defaults to NULL.
#'
#' @return `data.frame`
#' @noRd
make_component_lookup <- function(rsid, company_id, calculated_metrics = NULL) {
  # Get dimension and metric lookup tables
  dims <- aw_get_dimensions(rsid = rsid, company_id = company_id)
  mets <- aw_get_metrics(rsid = rsid, company_id = company_id)

  # pull out the calculated metrics
  if (length(calculated_metrics) > 0) {
    cms <- aw_get_calculatedmetrics(company_id = company_id, filterByIds = calculated_metrics)
    dimmets <- rbind(dims[c("id", "name")],
                     mets[c("id", "name")],
                     cms[c("id", "name")])
  } else {
    dimmets <- rbind(dims[c("id", "name")],
                     mets[c("id", "name")])
  }

  dimmets
}


#' Check if components are recognized by API
#'
#' Find components that aren't found in a lookup.
#'
#' @param component Character vector of component IDs
#' @param lookup Data frame containing ID column to compare names to
#'
#' @return Character vector of components that weren't found
#' @noRd
invalid_component_names <- function(component, lookup) {
  if (is.null(lookup$id)) {
    stop("Invalid lookup, missing 'id' column")
  }

  component[!(component %in% lookup$id)]
}
