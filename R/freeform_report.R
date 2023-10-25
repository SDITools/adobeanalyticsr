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
#' data in Analysis Workspace.
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
#' [aw_get_dimensions()], [aw_get_metrics()], [aw_get_calculatedmetrics()],
#' [aw_segment_table()]
#'
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used. Use [aw_get_reportsuites()] to get a list of available `rsid` values.
#' @param date_range A length-2 vector with a start date and an end date.
#'   `POSIXt` objects are sent as is, for fine control over the date range.
#'   Numeric values are automatically converted to dates.
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
#' @param check_components Specifies whether to check the validity of metrics and
#' dimensions before running the query. This defaults to `TRUE`, which triggers
#' several additional API calls behind the scenes to retrieve all dimensions and
#' metrics from the API. This has a nominal performance impact and may not be
#' ideal if you are running many queries. If you have many queries, consider
#' implementing validity checking through other means (manually or within the
#' code) and then set this value to `FALSE`.
#'
#' @return A data frame with the specified dimensions and metrics.
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
  if (company_id == "") stop("'company_id' is blank")
  if (rsid == "") stop("'rsid' is blank")

  if (all(is.na(segmentId))) segmentId <- NULL

  # Repeated dimensions will cause an infinite loop
  if (length(dimensions) > length(unique(dimensions))) {
    stop("List of dimensions is not unique")
  }
  # No harm in repeated metrics, simply take the unique ones
  metrics <- unique(metrics)


  # Component lookup checks
  # The component checking is optional, in case speed is a priority
  if (check_components | prettynames) {
    comp_lookup <- make_component_lookup(rsid,
                                         company_id,
                                         metrics[is_calculated_metric(metrics)])
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


  # Define the query with a query spec
  # This also standardizes arguments (recycling, etc.)
  query_spec <- make_query_spec(
    rsid = rsid,
    company_id = company_id,
    dimensions = dimensions,
    metrics = metrics,
    date_range = date_range,
    segment_id = segmentId,
    limit = top,
    page = page,
    include_unspecified = include_unspecified,
    dimensionSort = "asc",
    search = search,
    sort = metricSort
  )


  # Estimate requests and reset global counter
  n_requests <- estimate_requests(qs_top(query_spec))
  if (n_requests > 20) {
    initialize_global_counter(n_requests)
  } else {
    kill_global_counter()
  }

  # Make requests
  message("Requesting data...", appendLF = TRUE)
  output_data <- get_req_data(
    qs = query_spec,
    index = 1,
    item_ids = NULL,
    debug = debug
  )
  message("Done!")
  message(glue::glue("Returning {nrow(output_data)} x {ncol(output_data)} data frame"))

  output_data <- convert_date_columns(output_data)

  if (prettynames) {
    output_data <- dplyr::select(output_data,
                                 all_of(pretty_comp_names))
  }

  output_data
}





#' Calculate queries to be completed
#'
#' The number of queries required to complete a request. Also useful for vetting
#' a query to find an efficient dimension order.
#'
#' @param top Top argument, essentially the number of rows returned from
#' each query (can be inaccurate when fewer rows returned, but mostly correct)
#'
#' @return Number of queries needed to get top
#' @noRd
n_queries <- function(top) {
  top_ind <- c(1, top[-length(top)])
  sum(cumprod(top_ind))
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
  queries <- n_queries(top)

  if (length(top) > 1) {
    # I reckon about 1 second per query
    # sec
    est_secs <- round(queries * 1.1, digits = 0)
    # min
    est_mins <- round(est_secs/60, digits = 0)
    # hour
    est_hours <- round(est_mins / 60, digits = 1)

    if (est_secs < 60) {
      message_text <- glue::glue("{est_secs}sec.")
    } else if (est_mins < 60) {
      message_text <- glue::glue("{est_mins}min.")
    } else {
      message_text <- glue::glue("{est_hours}hr.")
    }

    message('Estimated runtime: ', message_text)
  }

  queries
}




#' Initializes the global decile query list
#'
#' This is used for generating the progress bar on long queries.
#'
#' @param n_queries Number of queries to be completed
#'
#' @return Query quantiles, invisibly
#' @noRd
initialize_global_counter <- function(total_queries) {
  prog_format <- "Progress [:bar] :percent in :elapsed"

  .adobeanalytics$prog_bar <- progress::progress_bar$new(total = total_queries,
                                                         format = prog_format,
                                                         clear = FALSE)
  invisible(total_queries)
}


#' Kill global counter
#'
#' Tears down the global counter
#'
#' @return NULL
#' @noRd
kill_global_counter <- function() {
  .adobeanalytics$prog_bar <- NULL
  NULL
}


#' Increment global counter
#'
#' Send a `tick` to the progress bar (see `progress::progress_bar`).
#' If no progress bar is initialized, do nothing.
#'
#' @return NULL
#' @noRd
increment_global_counter <- function() {
  if (!is.null(.adobeanalytics$prog_bar)) {
    .adobeanalytics$prog_bar$tick()
  }

  NULL
}



#' Convert date columns to date objects
#'
#' @param dat Data frame
#'
#' @return Data frame with date columns as dates
#' @noRd
convert_date_columns <- function(dat) {
  # change time variables from character strings
  if("daterangeminute" %in% colnames(dat)) {
    dat[names(dat) == 'daterangeminute'] <- lubridate::parse_date_time(dat$daterangeminute, orders = "HM ymd")
  }
  if("daterangehour" %in% colnames(dat)) {
    dat[names(dat) == 'daterangehour'] <- lubridate::parse_date_time(dat$daterangehour, orders = "HM ymd")
  }
  if("daterangeday" %in% colnames(dat)) {
    dat[names(dat) == 'daterangeday'] <- as.Date(dat$daterangeday, format = '%b %d, %Y')
  }
  if("daterangeweek" %in% colnames(dat)) {
    dat[names(dat) == 'daterangeweek'] <- as.Date(dat$daterangeweek, format = '%b %d, %Y')
  }

  dat
}

