library(jsonlite)
library(magrittr)

jsoncars <- toJSON(mtcars, pretty = TRUE)
toJSON(mtcars, dataframe = "columns", pretty = TRUE)

list(
  rsid = jsonlite::unbox("adobersid"),
  globalFilters = list(list(
    type = jsonlite::unbox("dateRange"),
    dateRange = jsonlite::unbox("2017-12-31T00:00:00.000/2018-01-06T23:59:59.999")
  )),
  dimension = jsonlite::unbox("variables/daterangeday"),
  settings = list(dimensionSort = jsonlite::unbox("asc"))
) %>%
  toJSON(pretty = TRUE)


# Breaking into parts
rsid <- jsonlite::unbox("adobersid")
globalFilters <- list(list(
  type = jsonlite::unbox("dateRange"),
  dateRange = jsonlite::unbox("2017-12-31T00:00:00.000/2018-01-06T23:59:59.999")
))
dimension <- jsonlite::unbox("variables/daterangeday")
settings <- list(dimensionSort = jsonlite::unbox("asc"))
metricContainer <- list(
  metrics = list(list(
    columnId = unbox("0"),
    id = unbox("metrics/pageviews"),
    filters = "0"
  )),
  metricFilters = list(list(
    id = unbox("0"),
    type = unbox("dateRange"),
    dateRange = unbox("2017-12-31T00:00:00.000/2018-01-06T23:59:59.999")
  ))
)

# A good way to serialize (globalFields would be passed in as an argument)
globalFields <- list(
  rsid = rsid,
  globalFilters = globalFilters,
  settings = settings
)

list(
  rsid = globalFields$rsid,
  globalFilters = globalFields$globalFilters,
  settings = globalFields$settings,
  metricContainer = metricContainer,
  dimension = dimension
) %>%
  toJSON(pretty = TRUE)


# Serializing metric filters ------
data.frame(
  id = as.character(c(0, 1, 2, 3)),
  type = "breakdown",
  dimension = rep(paste("variables", c("daterangeday", "page"), sep = "/"), each = 2),
  itemId = as.character(rep(c("12345", "67890"), each = 2))
) %>%
  toJSON(pretty = TRUE)


# Search terms -----
clause <- "BEGIN-WITH 'searchterm'"
grepl("BEGINS-WITH|STARTS|ENDS", clause)



# Global filters constructor -----
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

global_filter_elem(type = "daterange",
                   dateRange = "really-long-daterange-string")

global_filter_elem(segmentId = "segid",
                   type = "segment")


global_filter(
  type = c("dateRange", "segment"),
  segmentId = c(NA, "somesegmentid"),
  dateRange = c("somedaterange", NA)
)

global_filter(
  type = c("dateRange"),
  dateRange = c("somedaterange")
)



# Settings constructor ----
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

req_settings(0, 0, "return-nones")


# Metric field constructor -----
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

metric_elem(id = "metrics/pageviews",
            columnId = "0",
            sort = "asc")

metric_elems(id = c("metrics/pageviews", "metrics/daterangeday"),
            columnId = c("pageviews", "daterangeday"),
            sort = c("asc", NA),
            filters = c("evar5", "evar1")) %>%
  toJSON(pretty = TRUE, auto_unbox = TRUE)



# Metric filter constructor ----

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

metric_filters(
  id = as.character("page"),
  type = "breakdown",
  dimension = "variables/page",
  itemId = as.character("12345")
)

metric_filters(
  id = as.character(0),
  type = "dateRange",
  dateRange = "somedaterange"
)




# Encompassing metrics and filters with metric container -----
# For any one call, there will be 1 metric filter for each dimension,
# however many metrics each containing the _same_ filter. (We're imposing
# a restriction here. The API supports more fine-grained breakdowns.)

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
  metrics <- paste("metrics", metrics, sep = "/")
  if (!is.null(dimensions)) {
    dimensions <- paste("variables", dimensions, sep = "/")
    filter_ids <- dimensions
  } else {
    filter_ids <- "daterange"
  }

  # Sort by first metric
  sort_list <- rep(NA, times = length(dimensions))
  sort_list[1] <- sort

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
                       filter = filter_ids,
                       sort = sort_list)

  list(
    metrics = mets,
    metricFilters = met_filters
  )
}

metric_container(
  metrics = c("pageviews", "visitors"),
  type = "breakdown",
  sort = "desc",
  dimensions = c("evar5", "evar11"),
  itemIds = c("123456789", "987654321")
) %>%
  toJSON(pretty = TRUE, auto_unbox = TRUE)

metric_container(
  metrics = c("pageviews", "visitors"),
  type = "dateRange",
  sort = "desc",
  dimensions = NULL,
  itemIds = NULL,
  dateRange = "Today - Tomorrow"
) %>%
  toJSON(pretty = TRUE, auto_unbox = TRUE)






# Recreating a request -----
rsid <- "adbedocrsid"
gf <- global_filter(type = "dateRange", dateRange = "2020-12-31T00:00:00.000/2021-01-06T23:59:59.999")

metricContainer <- metric_container(
  metrics = "pageviews",
  type = "breakdown",
  sort = "desc",
  dimensions = "evar1",
  itemIds = c("743855946")
)

dimensions <- "variables/evar2"

settings <- req_settings(dimensionSort = "asc", limit = 5, page = 0, nonesBehavior = "exclude-nones")

final_request <- list(
  rsid = rsid,
  globalFilters = globalFilters,
  metricContainer = metricContainer,
  dimensions = dimensions,
  settings = settings
)

toJSON(final_request, pretty = TRUE, auto_unbox = TRUE)



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
#' @return Full request
make_request <- function(rsid,
                         global_filter,
                         dimension,
                         settings,
                         metric_container) {
  list(
    rsid = rsid,
    globalFilters = global_filter,
    metricContainer = metric_container,
    dimension = dimension,
    settings = settings
  )
}



# Recursive calling function -----

#' Recursively query for data
#'
#' @param current_dim Current dimension being queried
#' @param item_ids Item IDs for previous dimensions
#' @param dimensions All dimensions to be queried
#' @param metrics Metrics in the request
#' @param rsid Reportsuite ID
#' @param global_filter Global filter list
#' @param settings Settings list
#' @param client_id Client ID
#' @param client_secret Client secret
#' @param company_id Company ID
#' @param debug Whether to debug
#' @param sort How to sort results
#' @param top Top N items to get. Assumes input is same length as dimensions.
#' @param page Which page of results to get. Assumes input is same length as
#'   dimensions.
#'
#' @return Data frame
#' @noRd
get_req_data <- function(current_dim,
                         item_ids,
                         dimensions,
                         metrics,
                         rsid,
                         global_filter,
                         settings,
                         client_id,
                         client_secret,
                         company_id,
                         debug,
                         sort,
                         top,
                         page) {
  # TODO Encapsulate common bit of this?
  # TODO Simplify number of arguments?
  pos_current_dim <- match(current_dim, dimensions)
  previous_dims <- dimensions[seq_len(pos_current_dim - 1)]

  if (length(previous_dims) == 0) {
    previous_dims <- NULL
    dateRange <- global_filter[[1]]$dateRange
    type <- "dateRange"
  } else {
    dateRange <- NULL
    type <- "breakdown"
  }

  mc <- metric_container(
    metrics = metrics,
    type = type,
    sort = sort,
    dimensions = previous_dims,
    itemIds = item_ids,
    dateRange = dateRange
  )

  # Set top for this query
  settings$limit <- top[pos_current_dim]
  settings$page <- page[pos_current_dim]

  req <- make_request(
    rsid = rsid,
    global_filter = global_filter,
    dimension = paste("variables", current_dim, sep = "/"),
    settings = settings,
    metric_container = mc
  )

  message("Requesting data...", appendLF = FALSE)
  data <- fromJSON(aw_call_data(
    req_path = "reports/ranked",
    body = req,
    debug = debug,
    company_id = company_id,
    client_id = client_id,
    client_secret = client_secret
  ))
  message(sample(c("Fuck yeah!", "Nicely done!", "You're the shit!"), 1))


  # Base case
  if (pos_current_dim == length(dimensions)) {
    df <- data$rows
    dplyr::rename(df, !!current_dim := value)
  }
  # Recursive case
  else {
    next_dim <- dimensions[pos_current_dim + 1]
    dim_items <- data$rows[c("itemId", "value")]
    dim_items$recent_dim <- current_dim
    if (is.null(item_ids)) item_ids <- character()

    purrr::pmap_dfr(dim_items, function(itemId, value, recent_dim) {
      get_req_data(current_dim = next_dim,
                   item_ids = c(item_ids, itemId),
                   dimensions = dimensions,
                   metrics = metrics,
                   rsid = rsid,
                   global_filter = global_filter,
                   settings = settings,
                   client_id = client_id,
                   client_secret = client_secret,
                   company_id = company_id,
                   debug = debug,
                   sort = sort,
                   top = top,
                   page = page) %>%
        dplyr::mutate(!!recent_dim := value)
    }) %>%
      select(all_of(dimensions), data)
  }
}

get_req_data(
  current_dim = "evar5",
  item_ids = NULL,
  dimensions = c("evar5", "prop11"),
  metrics = "visits",
  rsid = "audiagaoa-t1-t2-t3-global-prod",
  global_filter = gf,
  settings = settings,
  client_id = Sys.getenv("AW_CLIENT_ID"),
  client_secret = Sys.getenv("AW_CLIENT_SECRET"),
  company_id = "audiag5",
  debug = FALSE,
  sort = "desc",
  top = c(5, 10),
  page = c(0, 0)
)





aw_freeform_table <- function(company_id = Sys.getenv("AW_COMPANY_ID"),
                              rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                              date_range = c(Sys.Date()-30, Sys.Date()-1),
                              dimensions = c('page', 'lasttouchchannel', 'mobiledevicetype'),
                              metrics = c("visits", "visitors"),
                              top = c(5),
                              page = 0,
                              filterType = 'breakdown',
                              segmentId = NA,
                              metricSort =  'desc',
                              include_unspecified = TRUE,
                              search = NA,
                              prettynames = FALSE,
                              debug = FALSE
)