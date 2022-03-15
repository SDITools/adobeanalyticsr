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
#'   ID given in the `metricFilters` field of metric container
#' @param sort Sorting directing, typically only applied to one metric
#'
#' @return List, one metric element
#' @noRd
#' @examples
#' metric_elems(id = c("met1", "met2"),
#'              columnId = as.character(1:2),
#'              filters = list("one", c("one", "two")),
#'              sort = c("asc", NA))
metric_elems <- function(id,
                         columnId,
                         filters = NULL,
                         sort = NULL) {
  # Input: character vector of filters
  # Output: List of filters, one filter for each element of ID
  if (!is.null(filters) & length(filters) > 1 & !is.list(filters)) {
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



#' Create metric filter data frame
#'
#' Combines elements into a single metric filter data frame. Automatically
#' generates an ID column for use with matching to the metric fields.
#'
#' @param type Type
#' @param dimension Dimensions
#' @param itemId Item IDs for those dimensions
#' @param dateRange Date range
#' @param segmentId segment IDs
#'
#' @return Data.frame containing the necessary fields to generate the metric
#' filters field of the metric container
#'
#' @noRd
#' @examples
#'
#' metric_filters(
#'   type = c("segment", "breakdown", "breakdown", "dateRange"),
#'   segmentId = c("s1234567890_09583204824324"),
#'   dimension = c("evar45", "prop11"),
#'   itemId = c("1234", "5678"),
#'   dateRange = "today/tomorrow"
#' )
#'
#'
#' metric_filters(
#'   type = c("segment", "breakdown"),
#'   segmentId = c("s1234567890_09583204824324"),
#'   dimension = "evar45",
#'   itemId = "1234"
#' )
metric_filters <- function(type,
                           dimension = NULL,
                           itemId = NULL,
                           dateRange = NULL,
                           segmentId = NULL) {
  if (length(dimension) != length(itemId)) {
    stop("Mismatch between dimensions and itemIds in metric filter")
  }

  stopifnot(length(dateRange) <= 1)
  stopifnot(length(type) == length(c(dimension, dateRange, segmentId)))

  dr <- data.frame(
    id = type[type == "dateRange"],
    type = type[type == "dateRange"],
    dateRange = dateRange,
    stringsAsFactors = FALSE
  )

  dims <- data.frame(
    id = dimension,
    type = type[type == "breakdown"],
    dimension = dimension,
    itemId = itemId,
    stringsAsFactors = FALSE
  )

  segs <- data.frame(
    id = segmentId,
    type = type[type == "segment"],
    segmentId = segmentId,
    stringsAsFactors = FALSE
  )

  dplyr::bind_rows(dr, dims, segs)
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
#' @param segmentIds List (or vector) of segment IDs the same length as the
#' metrics. These will be added to metrics as appropriate.
#' @param sort Direction to sort in, one of "asc", "desc". Applied only to first
#'   metric.
#' @param dimensions Dimensions to apply as filters. Must be same length as IDs.
#' @param itemIds Dimension item IDs. Must be same length as dimensions.
#' @param dateRange If type is dateRange, the dateRange to use.
#'
#' @return Metric container list
#' @noRd
#'
#' @examples
#'
#' metric_container(
#'   metrics = c("met1", "met2"),
#'   type = c("breakdown", "dateRange"),
#'   sort = c("asc", NA),
#'   dimensions = c("evar45"),
#'   itemIds = c("1234"),
#'   dateRange = "yesterday/today"
#' )
#'
#' metric_container(
#'   metrics = c("met1", "met2"),
#'   sort = c("asc", NA),
#'   dimensions = c("evar45"),
#'   itemIds = c("1234"),
#'   segmentIds = list(NA, c("s1234_5555", "s1234_9999"))
#' )
metric_container <- function(metrics,
                             metricIds,
                             sort,
                             dimensions = NULL,
                             itemIds = NULL,
                             segmentIds = NULL,
                             dateRange = NULL) {
  sort <- na_fill_vec(sort, len = length(metrics))

  # Format metrics for API request
  metrics[!is_calculated_metric(metrics)] <- paste("metrics",
                                               metrics[!is_calculated_metric(metrics)],
                                               sep = "/")

  # Format dimensions for API request
  if (!is.null(dimensions)) {
    dimensions <- paste("variables", dimensions, sep = "/")
  }

  # Get a list of unique segment IDs needed for filtering
  filter_segids <- unique(unlist(segmentIds))
  filter_segids <- filter_segids[!is.na(filter_segids)]


  # Derive type argument
  type <- c(
    rep("dateRange", length(dateRange)),
    rep("breakdown", length(dimensions)),
    rep("segment", length(filter_segids))
  )


  met_filters <- metric_filters(
    type = type,
    dimension = dimensions,
    itemId = itemIds,
    segmentId = filter_segids,
    dateRange = dateRange
  )

  # Segment IDs can be applied on a per metric basis
  if (!is.null(segmentIds)) {
    stopifnot(length(metrics) == length(segmentIds))

    filter_ids <- purrr::map2(metrics, segmentIds, function(met, seg) {
      met_filters[met_filters$type %in% c("breakdown", "dateRange") | met_filters$segmentId %in% seg, "id"]
    })
  } else {
    filter_ids <- list(met_filters$id)
  }

  mets <- metric_elems(id = metrics,
                       columnId = metricIds,
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

