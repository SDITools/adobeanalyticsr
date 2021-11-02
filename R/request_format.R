#' Global filter element
#'
#' Both types of filter elements (dateRange, segment) are supported.
#'
#' @param String, one of 'daterange' or 'segment'
#' @param segmentId For segment, segment ID
#' @param dateRage For daterange, date range
#' @param id I'm actually not sure
#'
#' @return Properly formatted global filter element
#' @noRd
#' @examples
#' global_filter_elem(type = "daterange",
#'                    dateRange = "really-long-daterange-string")
#'
#' global_filter_elem(segmentId = "segid",
#'                    type = "segment")
global_filter_elem <- function(type,
                               segmentId = NULL,
                               dateRange = NULL,
                               id = NULL) {
  if (!is.null(segmentId) && is.na(segmentId)) segmentId <- NULL
  if (!is.null(dateRange) && is.na(dateRange)) dateRange <- NULL

  if (type == "daterange" && is.null(dateRange)) stop("Missing daterange in global filter element", call. = FALSE)
  if (type == "segment" && is.null(segmentId)) stop("Missing segment ID in global filter element", call. = FALSE)
  if (is.null(segmentId) && is.null(dateRange)) stop("No content for global filter element", call. = FALSE)
  if (!is.null(segmentId) && !is.null(dateRange)) stop("Only one of segmentId or dateRange may be specified in global filter element", call. = FALSE)

  purrr::compact(list(
    id = id,
    type = type,
    segmentId = segmentId,
    dateRange = dateRange
  ))
}


#' Generate a global filter
#'
#' Vectorized global filter generator. Generates one or more global filter
#' elements with `global_filter_elem`. This might not be as useful as calling
#' `global_filter_elem` directly.
#'
#'
#' @param type Character, vector of filter types
#' @param segmentId Character, vector of segment IDs
#' @param dateRange Character, vector of date ranges
#'
#' @return List of global filter elements
#' @noRd
global_filter <- function(type,
                          segmentId = NULL,
                          dateRange = NULL) {
  items <- purrr::compact(list(type = type, segmentId = segmentId, dateRange = dateRange))
  purrr::pmap(items, global_filter_elem)
}


#' Request settings
#'
#' @param limit Numeric, number of results to display
#' @param page Numeric, which page to return
#' @param nonesBehavior How to treat "Unspecified"
#' @param ... Other settings, not error checked
#'
#' @return List
#' @noRd
req_settings <- function(limit,
                         page,
                         nonesBehavior,
                         ...) {
  assertthat::assert_that(
    is.numeric(limit),
    is.numeric(page),
    is.character(nonesBehavior),
    nonesBehavior %in% c("return-nones", "exclude-nones")
  )


  list(
    limit = limit,
    page = page,
    nonesBehavior = nonesBehavior,
    ...
  )
}


#' Construct a metric element
#'
#' Metric elements are lists composed of two mandatory fields and two optional
#' fields.
#'
#' @param id Metric ID
#' @param columnId Assigned column, always the same for each metric
#' @param filters Character vector of metric filter IDs to include, identified
#'   by ID given in the `metricFilters` field
#' @param sort Sorting directing, typically only applied to one metric
#'
#' @return List, one metric element
#' @noRd
metric_elem <- function(id,
                        columnId,
                        filters = NULL,
                        sort = NULL) {
  assertthat::assert_that(
    is.character(id),
    is.character(columnId)
  )
  if (!is.null(filters)) {
    assertthat::assert_that(
      is.character(filters)
    )
    filters <- I(filters)
  }
  if (!is.null(sort)) {
    if (is.na(sort)) sort <- NULL
    else sort <- match.arg(sort, c("asc", "desc"))
  }

  purrr::compact(list(
    id = id,
    columnId = columnId,
    filters = filters,
    sort = sort
  ))
}


#' Make metric elements
#'
#' Vectorized version of metric_elem that handles NA values and some other
#' things.
#'
#' @param id Vector of metric IDs
#' @param columnId Assigned columns, should be always the same for each metric
#' @param filter List of metric filters to include in each metric, identified by
#'   ID given in the `metricFilters` field
#' @param sort Sorting directing, typically only applied to one metric
#'
#' @return List, one metric element
#' @noRd
metric_elems <- function(id,
                         columnId,
                         filters = NULL,
                         sort = NULL) {
  # Input: character vector of filters
  # Output: List of filters, one filter for each element of ID
  if (!is.null(filters) & length(filters) > 1) {
    id_len <- length(id)
    filters <- list(filters)[rep(1, id_len)]
  }

  elems <- purrr::compact(list(
    id = id,
    columnId = columnId,
    filters = filters,
    sort = sort
  ))

  purrr::pmap(elems, metric_elem)
}


#' Make a metric filter data frame
#'
#' @description
#' Construct a metric filter data frame. The intended use is to row bind several
#' of these together to form the full `metricFilters` field. Thus, in each
#' call, you are restricted to:
#'
#' - A vector of filter IDs
#' - A type (dateRange or breakdown)
#' - Depending on type:
#'   - One dimension with the same number of item IDs as filter IDs (usu. 1)
#'   - One daterange to be applied to all filters
#'
#' @param id Metric filter ID, assigned by form creator
#' @param type Filter type, one of "dateRange" or "breakdown"
#' @param dimension Optional, dimension ID
#' @param itemId Optional, dimension item ID
#' @param dateRange Optional, date range
#'
#' @return data.frame
#' @noRd
metric_filters <- function(id,
                           type,
                           dimension = NULL,
                           itemId = NULL,
                           dateRange = NULL) {
  assertthat::assert_that(
    is.character(id),
    length(dimension) < 2,
    length(dateRange) < 2
  )
  type <- match.arg(type, c("dateRange", "breakdown"))

  if (type == "dateRange") {
    if (is.null(dateRange)) stop("No date range provided for dateRange metric filter",
                                 call. = FALSE)
  }

  if (type == "breakdown") {
    if (is.null(dimension) || is.null(itemId)) stop("No dimension/item ID given for breakdown metric filter",
                                                    call. = FALSE)
  }

  purrr::compact(list(
    id = id,
    type = type,
    dimension = dimension,
    itemId = itemId,
    dateRange = dateRange
  )) %>%
    data.frame()
}



#' Encompass metrics and filters in a container
#'
#' @description
#' For any call, there will be 1 metric filter for each dimension, and this
#' filter is applied to all metrics. This function takes care of the metric
#' filter ID, since it is not needed outside the query (i.e., it's not returned
#' in the response).
#'
#' This function also fixes the names of metrics and dimensions, so you can
#' pass in normal values. You know, for user friendliness.
#'
#' @param metrics Metric names in the order they were requested
#' @param type Type of filter to apply. One of "dateRange" or "breakdown"
#' @param sort Direction to sort in, one of "asc", "desc". Applied only to first
#'   metric.
#' @param dimensions Dimensions to apply as filters. Must be same length as IDs.
#' @param itemIds Dimension item IDs. Must be same length as dimensions.
#' @param dateRange If type is dateRange, the dateRange to use.
#'
#' @return Metric container list
#' @noRd
metric_container <- function(metrics,
                             type,
                             sort,
                             dimensions = NULL,
                             itemIds = NULL,
                             dateRange = NULL) {
  # Error checking happens in lower level functions, should probably move them higher
  # Format for API request
  metrics[!is_calculated_metric(metrics)] <- paste("metrics",
                                               metrics[!is_calculated_metric(metrics)],
                                               sep = "/")

  if (!is.null(dimensions)) {
    dimensions <- paste("variables", dimensions, sep = "/")
    filter_ids <- dimensions
  } else {
    filter_ids <- "daterange"
  }


  filter_components <- purrr::compact(list(
    id = filter_ids,
    type = type,
    dimension = dimensions,
    itemId = itemIds,
    dateRange = dateRange
  ))

  met_filters <- purrr::pmap_dfr(filter_components, metric_filters)

  mets <- metric_elems(id = metrics,
                       columnId = as.character(seq_along(metrics)),
                       filters = filter_ids,
                       sort = sort)

  list(
    metrics = mets,
    metricFilters = met_filters
  )
}


#' Create requests for item IDs
#'
#' Mostly this function is for convenience when dealing with the proper field
#' names.
#'
#' @param global_filter Global filter data structure
#' @param dimension Dimension to get for the breakdown
#' @param settings List of settings
#' @param metric_container Metric container
#'
#' @return Full request list structure
#' @noRd
make_request <- function(rsid,
                         global_filter,
                         dimension,
                         settings,
                         metric_container,
                         search = NULL) {
  purrr::compact(list(
    rsid = rsid,
    globalFilters = global_filter,
    metricContainer = metric_container,
    dimension = dimension,
    settings = settings,
    search = search
  ))
}

