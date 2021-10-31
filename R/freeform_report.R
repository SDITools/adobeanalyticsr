#' Get a freeform table
#'
#' Get a report analogous to a **Freeform Table** visualization in Analysis Workspace. The function uses
#' the arguments to construct and execute a JSON-based query to the Adobe Analytics API and then returns
#' the results as a data frame.
#'
#' @details
#'
#' This function is based on the **Freeform Table** visualization in Analysis Workspace. It is accessing
#' the same API call type that is used to generate those visualizations.
#'
#' ## Dimension Ordering
#'
#' Adobe Analytics only queries one dimension at a time, even though the results get returned in a single data
#' frame (or table in the case of Analysis Workspace). The more dimensions are included in the report--the more
#' breakdowns of the data--the more queries are required. As a result, the _order_ of the dimensions _can_
#' have a dramatic impact on the total query time, even if the resulting data is essentially identical.
#'
#' One way to understand this is to consider how much dragging and dropping would be required to return the
#' data in Analysis Workspace _if you were not able to <Shift>-<click> to highlight multiple values before
#' dragging a new dimension to break down existing values_.
#'
#' Consider a scenario where you are pulling metrics for the last 30 days (`daterangeday`) for **Mobile Device Type**
#' (`mobiledevicetype`), which has 7 unique values. Setting `dimensions = c("daterangeday", "mobiledevicetype")`
#' would make one query to get the values of the 30 days included. The query would then run a separate query
#' for _each of those 30 days_ to get the `mobiledevicetype` results for each day. So, this would be **31 API calls**.
#'
#' If, instead, the function was called with the `dimension` values reversed (`dimensions = c("mobiledevicetype", "daterangeday")`), then
#' the first query would return the 7 `mobiledevicetype` values, and then would run an additional query for each of
#' those _7 mobile device type values_ to return the results for the 30 days within each device type. This would be only **7 API calls**.
#'
#' Strategically ordering dimensions--and then wrangling the resulting data set as needed--is one of the best
#' ways to improve query performance.
#'
#' ## Date Handling
#'
#' Date handling has several special characteristics that are worth getting familiar with:
#' * The API names for day, week, month, etc. are prepended with `daterange`, so daily data uses
#' `daterangeday`, weekly data uses `daterangeweek`, monthly data uses `daterangemonth`, etc.
#' * When setting the argument for `top`, if the first (or only) `dimension` value is a `daterange...` object,
#' then, if this argument is not explicitly specified _or_ if it uses only a single value (e.g., `top = 10`),
#' the function will still return all of the values that fall in that date range. For instance, if the
#' `date_range` was set for a 30-day period and the first `dimension` value was `daterangeday`, _and_ no value
#' is specified for `top`, rather than simply returning the first 5 dates in the range, all 30 days will be
#' returned. In the same scenario, if `top = 10` was set, then all 30 days would still be returned, and the
#' `10` would simply be applied to the additional dimensions.
#' * If you want to return all of the date/time values but then have specific control over the number of
#' values returned for each of the drilldown dimensions, then set `0` as the first value in the `top`
#' argument and then specify different numbers for each breakdown (e.g., `top = c(0, 3, 10)` would return
#' all of the date/time values for the specified `date_range`, the top 3 values for the second specified
#' `dimension`, and then the top 10 values for each of the next dimension's results).
#' * If you are using a `daterange...` value _not_ as the first dimension, then simply using `0` at the
#' same level in the `top` argument specification will return all of the values for that date/time value.
#'
#' ## Search/Filtering
#'
#' There are powerful filtering abilities within the function. However, to support that power requires a
#' syntax that can feel a bit cumbersome for simple queries. **_Note:_** search filters are case-insensitive.
#' This is Adobe Analytics API functionality and can not be specified otherwise in queries.
#'
#' The `search` argument takes a vector of search strings, with each value in the vector corresponding to
#' the `dimension` value that is at the same position. These search strings support a range of operators,
#' including `AND`, `OR`, `NOT`, `MATCH`, `CONTAINS`, `BEGINS-WITH`, and `ENDS-WITH`.
#'
#' The default for any search string is to use `CONTAINS`. Consider a query where
#' `dimensions = c("mobiledevicetype", "lasttouchchannel")`:
#'
#' - `search = "CONTAINS 'mobile'"` will return results where `mobiledevicetype` contains "mobile", so would return all rows for **Mobile Phone**.
#' - This could be shortened to `search = "'mobile'"` and would behave exactly the same, since `CONTAINS` is the default operator
#' - `search = c("CONTAINS 'mobile'", "CONTAINS 'search'")` will return results where `mobiledevicetype` contains "mobile" and, within those results, results where `lasttouchchannel` contains "search".
#' - `search = c("(CONTAINS 'mobile') OR (CONTAINS 'tablet')", "(MATCH 'paid search')")` will return results where `mobiledevicetype` contains "mobile" _or_ "tablet" and, within those results, will only include results where `lasttouchchannel` exactly matches "paid search" (but is case-insensitive, so would return "Paid Search" values).
#'
#' @seealso [get_me()], [aw_get_reportsuites()], [aw_get_segments()],
#' [aw_get_dimensions()], [aw_get_metrics()], [aw_get_calculatedmetrics()]
#'
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used. Use [aw_get_reportsuites()] to get a list of available `rsid` values.
#' @param date_range A vector containing the start and end date for the report as **Date** objects.
#' @param metrics A character vector of metrics. Use [aw_get_metrics()] and [aw_get_calculatedmetrics()]
#' to get a list of available `metrics` IDs.
#' @param dimensions A character vector of dimensions. There is currently a limit of 20 dimension
#' breakdowns. Each dimension value that gets broken down by another dimension requires an additional API
#' call, so the more dimensions that are included, the longer the function will take to return results.
#' This is how the Adobe Analytics API works. Use [aw_get_dimensions()] to get a list of available
#' `dimensions` IDs.
#' @param top The number of values to be pulled for each dimension. The default is 5 and the "top" is based on
#' the first `metric` value (along with `metricSort`). If there are multiple dimensions, then this argument can
#' either be a vector that includes the number of values to include at each level (each breakdown) or, if a single
#' value is used, then that will be the maximum number of values to return at each level. See the **Details** for
#' information on the unique handling of `daterange...` values.
#' @param page Used in combination with `top` to return the next page of results. Uses 0-based numbering (e.g.,
#' `top = 50000` and `page = 1` will return the top 50,000 items _starting at 50,001_).
#' @param metricSort Pre-sorts the table by metrics. Values are either `asc` (ascending) or `desc` (descending).
#' @param filterType This is a placeholder argument for use as additional functionality is added to the package.
#' Currently, it defaults to `breakdown`, and that is the only supported value.
#' @param include_unspecified Whether or not to include **Unspecified** values in the results. This is the equivalent
#' of the **Include Unspecified (None)** checkbox in freeform tables in Analysis Workspace. This defaults to `TRUE`,
#' which includes **Unspecified** values in the results.
#' @param segmentId A single segment ID or a vector of multiple segment IDs to apply to the overall report.
#' If multiple `segmentId` values are included, the segments will be effectived ANDed together, just as if
#' multiple segments were added to the header of an Analysis Workspace panel. Use [aw_get_segments()]
#' to get a list of available `segmentId` values.
#' @param search Criteria to filter the results by one or more dimensions. Searches are case-insenstive. Refer to
#' the **Details** for more information on constructing values for this argument.
#' @param prettynames A logical that determines whether the column names in the results use the API field name
#' (e.g., "mobiledevicetype", "pageviews") or the "pretty name" for the field (e.g., "Mobile Device Type",
#' "Page Views"). This applies to both dimensions and metrics. The default value is `FALSE`, which returns the
#' API field names. For custom eVars, props, and events, the non-pretty values are simply the variable number
#' (e.g., "evar2", "prop3", "event15"). If `TRUE`, undoes any efficiency gains
#' from setting `check_components` to `FALSE`.
#' @param debug Set to `TRUE` to publish the full JSON request(s) being sent to the API to the console when the
#' function is called. The default is `FALSE`.
#' @param check_components Logical, whether to check the validity of metrics and
#' dimensions before running the query. Defaults to `TRUE`, but causes
#' `aw_freeform_report` to request all dimensions and metrics from the API,
#' which may be inefficient if you're running many queries. If you have many
#' queries, it's more efficient to implement validity checking yourself on either
#' side of your queries.
#'
#' @return A data frame with dimensions and metrics.
#'
#' @export
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
                              debug = FALSE,
                              check_components = TRUE) {
  if (all(is.na(segmentId))) segmentId <- NULL
  search <- na_fill_vec(search, len = length(dimensions))

  # Component lookup checks
  # The component checking is optional, in case speed is a priority
  if (check_components | prettynames) {
    comp_lookup <- make_component_lookup(rsid, company_id, metrics)
    invalid_components <- invalid_component_names(component = c(dimensions, metrics),
                                                  lookup = comp_lookup)

    if (length(invalid_components > 0)) {
      invalid_components <- paste(invalid_components, collapse = ", ")
      stop(paste("Component(s) not found: ", invalid_components), call. = FALSE)
    }

    if (prettynames == TRUE) {
      pretty_comp_names <- c(dimensions, metrics)
      names(pretty_comp_names) <- comp_lookup$name[match(pretty_comp_names, comp_lookup$id)]
    }
  }


  # Make global filter
  timeframe <- make_timeframe(date_range[[1]], date_range[[2]])

  gf <- global_filter(
    type = c("dateRange", rep("segment", times = length(segmentId))),
    dateRange = c(timeframe, rep(NA, times = length(segmentId))),
    segmentId = c(NA, segmentId)
  )

  # Check search for at least one instance of a keyword
  if (!all(is.na(search))) {
    search_keywords <- c("AND", "OR", "NOT", "MATCH", "CONTAINS", "BEGINS-WITH", "ENDS-WITH")
    if (sum(grepl(paste(search_keywords, collapse = "|"), search)) == 0) {
      stop("Search field must contain at least one of: ", paste(search_keywords, collapse = ", "))
    }
  }

  # Set settings
  unspecified <- ifelse(include_unspecified, "return-nones", "exclude-nones")
  top <- top_daterange_number(top, dimensions, date_range)
  page <- vctrs::vec_recycle(page, size = length(dimensions))

  settings <- req_settings(
    limit = 0,    # Placeholder, limit set during query
    page = 0,     # Placeholder, page set during query
    nonesBehavior = unspecified,
    dimensionSort = "asc"
  )

  # Estimate requests
  n_requests <- estimate_requests(top)


  # Make requests
  message("Requesting data...", appendLF = FALSE)
  output_data <- get_req_data(
    current_dim = dimensions[1],
    dimensions = dimensions,
    item_ids = NULL,
    metrics = metrics,
    rsid = rsid,
    global_filter = gf,
    settings = settings,
    client_id = Sys.getenv("AW_CLIENT_ID"),
    client_secret = Sys.getenv("AW_CLIENT_SECRET"),
    company_id = company_id,
    debug = debug,
    sort = metricSort,
    top = top,
    page = page,
    search = search
  )
  message("Done!")
  message(glue::glue("Returning {nrow(output_data)} x {ncol(output_data)} data frame"))

  if (prettynames) {
    output_data <- dplyr::select(output_data,
                                 all_of(pretty_comp_names))
  }

  output_data
}

#' Check if metrics are custom
#'
#' @param metric Vector of metrics
#'
#' @return Logical, `TRUE` if metric is custom and `FALSE` otherwise
#' @noRd
is_custom_metric <- function(metric) {
  grepl('cm[1-9]*_*', metric)
}


#' Make a component lookup table
#'
#' @param rsid Reportsuite ID
#' @param company_id Company ID
#' @param metrics Vector of metric IDs, for getting calculated metrics
#'
#' @return `data.frame`
#' @noRd
make_component_lookup <- function(rsid, company_id, metrics) {
  # Get dimension and metric lookup tables
  dims <- aw_get_dimensions(rsid = rsid, company_id = company_id)
  mets <- aw_get_metrics(rsid = rsid, company_id = company_id)

  # pull out the calculated metrics
  cms_ids <- metrics[is_custom_metric(metrics)]

  if (length(cms_ids) > 0) {
    cms <- aw_get_calculatedmetrics(company_id = company_id, filterByIds = cms_ids)
    dimmets <- rbind(dims[c("id", "name")], mets[c("id", "name")], cms[c(2, 3)])
  } else {
    dimmets <- rbind(dims[c("id", "name")], mets[c("id", "name")])
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


#' Estimate number of requests for query
#'
#' Also calculates estimated runtime and sends it as a message to the console
#'
#' @param top Top argument
#'
#' @return Number of requests necessary to complete query
#' @noRd
estimate_requests <- function(top) {
  if (length(top) > 1) {
    toplength <- length(top)
    i <- product <- 1
    topestimate1 <- top[-toplength]
    topestimate1 <- append(topestimate1, 1, after = 0)

    for (i in seq(toplength)) {
      product <- product + prod(topestimate1[1:i])
    }
    # sec
    est_secs <- round((product-1)*.80, digits = 2)
    # min
    est_mins <- round(((product-1)*.80)/60, digits = 2)
    message('Estimated runtime: ', est_secs, 'sec./', est_mins, 'min.')
    # message(paste0('Estimating a total of ', product-2, ' API calls'))

    product
  }
}

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
  metrics <- paste("metrics", metrics, sep = "/")
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
#' @param search Search clause in final form
#'
#' @return Data frame
#'
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
                         page,
                         search = NULL) {
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

  # Set top, page, and search for this query
  settings$limit <- top[pos_current_dim]
  settings$page <- page[pos_current_dim]
  search_field <- list(clause = search[pos_current_dim] %||% NA)


  req <- make_request(
    rsid = rsid,
    global_filter = global_filter,
    dimension = paste("variables", current_dim, sep = "/"),
    settings = settings,
    metric_container = mc,
    search = search_field
  )


  data <- jsonlite::fromJSON(aw_call_data(
    req_path = "reports/ranked",
    body = req,
    debug = debug,
    company_id = company_id,
    client_id = client_id,
    client_secret = client_secret
  ))


  dimensions_so_far <- dimensions[seq(pos_current_dim, length(dimensions))]

  # Base case
  if (pos_current_dim == length(dimensions)) {
    # If no data is returned, data$rows is an empty list, so handle that
    output_data <- fix_missing_metrics(data$rows,
                                       n_metrics = length(metrics))

    output_data <- output_data %>%
      dplyr::rename(!!current_dim := value) %>%
      unpack_metrics(metrics)
  }
  # Recursive case
  else {
    # Abort recursion if response is empty
    if (identical(data$rows, list())) {
      output_data <- fix_missing_metrics(
        data$rows,
        n_metrics = length(metrics),
        dimensions = dimensions[pos_current_dim:length(dimensions)]
      ) %>%
        unpack_metrics(metrics)

      return(output_data)
    }

    next_dim <- dimensions[pos_current_dim + 1]
    dim_items <- data$rows[c("itemId", "value")]
    dim_items$recent_dim <- current_dim
    if (is.null(item_ids)) item_ids <- character()


    output_data <- purrr::pmap_dfr(dim_items, function(itemId, value, recent_dim) {
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
    })
  }

  output_data %>%
    select(all_of(dimensions_so_far), all_of(metrics))
}


#' Unpacks metric column
#'
#' @param df Data frame possibly containing a list column called `data`
#' @param metric_names Metric names in the order they appear in the list column
#'
#' @return `df` with list column unpacked
#' @noRd
unpack_metrics <- function(df, metric_names) {
  if (identical(df, data.frame())) {
    return(df)
  } else {
    if (is.list(df$data)) {
      data_list <- df$data
      df$data <- NULL

      data_df <- lapply(purrr::transpose(data_list), flatten_dbl) %>%
        stats::setNames(metric_names) %>%
        as.data.frame()

      df <- cbind(df, data_df)
    }
  }


  df
}


#' Expand missing metric data with NAs
#'
#'
#' @param df Data frame
#' @param n_metrics Number of metrics in request
#' @param dimensions Dimension columns to create. Defaults to `value`, which is
#'   what gets returned in the base case (leaf nodes) of recursive function. For
#'   recursive cases where no data is returned, `dimensions` should be the
#'   current dimension and all remaining dimensions.
#'
#' @return If `df` is a data frame, nothing is done to it. If it is an empty
#'   list, creates a data frame that imitates the response from the API, with
#'   a dimension column given by `dimensions` and a list column of metrics,
#'   where each row has length `n_metrics`.
#' @noRd
#' @examples
#' # Nothing done to data frames
#' fix_missing_metrics(data.frame(x = 1:10))
#'
#' # If no rows are returned, first argument is empty list
#' # Uses 'value' by default, for the leaf node cases
#' fix_missing_metrics(list(), 1)
#'
#' # You can override dimensions that get created with 'dimensions'
#' fix_missing_metrics(list(), 2, c("one", "two"))
fix_missing_metrics <- function(df, n_metrics, dimensions = "value") {
  if (identical(df, list())) {
    warning("Response contained no data; filling with NA", call. = FALSE)
    df <- as.list(rep(NA, length(dimensions)))
    df <- as.data.frame(df, col.names = dimensions)

    metric_list_col <- list(rep(NA, n_metrics))
    df$data <- metric_list_col
  }

  as.data.frame(df)
}




#' Fill a vector of a certain length with NA
#'
#' Similar to [vctrs::vec_recycle()], but fills remaining values with `NA`.
#'
#' @param x Vector
#' @param len Length to fill to
#'
#' @return A vector the same length as `len` with the difference made up by `NA`
#' @noRd
na_fill_vec <- function(x, len) {
  len_x <- length(x)
  if (len_x != len & len_x != 1) {
    stop("Vector has length !=1 but not `len`")
  } else if (len_x == len) {
    return(x)
  } else if (len_x == 1) {
    x[2:len] <- NA
  }

  x
}
