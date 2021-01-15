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
#' **Dimension Ordering**
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
#' **Date Handling**
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
#' **Search/Filtering**
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
#' * `search = "CONTAINS 'mobile'"` will return results where `mobiledevicetype` contains "mobile", so would return all rows for **Mobile Phone**.
#' * This could be shortened to `search = "'mobile'"` and would behave exactly the same, since `CONTAINS` is the default operator
#' * `search = c("CONTAINS 'mobile'", "CONTAINS 'search'")` will return results where `mobiledevicetype` contains "mobile" and, within those results, results where `lasttouchchannel` contains "search".
#' * `search = c("(CONTAINS 'mobile') OR (CONTAINS 'tablet')", "(MATCH 'paid search')")` will return results where `mobiledevicetype` contains "mobile" _or_ "tablet" and, within those results, will only include results where `lasttouchchannel` exactly matches "paid search" (but is case-insensitive, so would return "Paid Search" values).
#'
#' @seealso \code{\link{get_me}}, \code{\link{aw_get_reportsuites}}, \code{\link{aw_get_segments}},
#' \code{\link{aw_get_dimensions}}, \code{\link{aw_get_metrics}}, \code{\link{aw_get_calculatedmetrics}}
#'
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use \code{\link{get_me}} to get a list of available `company_id` values.
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used. Use \code{\link{aw_get_reportsuites}} to get a list of available `rsid` values.
#' @param date_range A vector containing the start and end date for the report as **Date** objects.
#' @param metrics A character vector of metrics. Use \code{\link{aw_get_metrics}} and \code{\link{aw_get_calculatedmetrics}}
#' to get a list of available `metrics` IDs.
#' @param dimensions A character vector of dimensions. There is currently a limit of 20 dimension
#' breakdowns. Each dimension value that gets broken down by another dimension requires an additional API
#' call, so the more dimensions that are included, the longer the function will take to return results.
#' This is how the Adobe Analytics API works. Use \code{\link{aw_get_dimensions}} to get a list of available
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
#' multiple segments were added to the header of an Analysis Workspace panel. Use \code{\link{aw_get_segments}}
#' to get a list of available `segmentId` values.
#' @param search Criteria to filter the results by one or more dimensions. Searches are case-insenstive. Refer to
#' the **Details** for more information on constructing values for this argument.
#' @param prettynames A Boolean that determines whether the column names in the results use the API field name
#' (e.g., "mobiledevicetype", "pageviews") or the "pretty name" for the field (e.g., "Mobile Device Type",
#' "Page Views"). This applies to both dimensions and metrics. The default value is `FALSE`, which returns the
#' API field names. For custom eVars, props, and events, the non-pretty values are simply the variable number
#' (e.g., "evar2", "prop3", "event15").
#' @param debug Set to `TRUE` to publish the full JSON request(s) being sent to the API to the console when the
#' function is called. The default is `FALSE`.
#'
#' @return A data frame with dimensions and metrics.
#'
#' @import assertthat
#' @import httr
#' @import httr
#' @import dplyr
#' @import stringr
#' @import purrr
#' @import tidyr
#' @import purrrlyr
#' @importFrom rlang :=
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate parse_date_time
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
                               debug = FALSE
                               )
{

# Prep Work for the api calls ------------------------------------------------------------------
  #Making sure metrics and dimensions are available.  Error handling.
  dims <- aw_get_dimensions(rsid = rsid, company_id = company_id)
  mets <- aw_get_metrics(rsid = rsid, company_id = company_id)
  #pull out the calculated metrics
  cms_ids <- metrics[grepl('cm[1-9]*_*', metrics)]
  if (length(cms_ids) > 0) {
    cms <- aw_get_calculatedmetrics(company_id = company_id, filterByIds = cms_ids)
    dimmets <- rbind(dims[c(1, 3)], mets[c(1, 3)], cms[c(2, 3)])
  } else {
    dimmets <- rbind(dims[c(1, 3)], mets[c(1, 3)])
  }
  finalnames <- c(dimensions, metrics)
  #include an error handler for metrics that do not appear in the list
  for (x in seq(finalnames)) {
    if (finalnames[[x]] %in% dimmets[, 1] == FALSE) {
      stop(paste0('\'', finalnames[[x]], '\' is not an available element'))
    }
  }
  #create the list of pretty named variables for the final output dataset
  if (prettynames == TRUE) {
    pnames <- function(x) {
      dimmets %>% dplyr::filter(finalnames[x] == id) %>% dplyr::pull(name)
    }
    prettyfinalnames <- map_chr(seq(finalnames), pnames)
  }
  #Identify the handling of unspecified
  if (include_unspecified == FALSE) {
    unspecified <- "exclude-nones"
  }
  if (include_unspecified == TRUE) {
    unspecified <- "return-nones"
  }
  itemId <- list(dimensions)

  finalnames_function <- function(level) {
    c(paste0('itemId_',dimensions[level]), dimensions[level])
  }

  prefinalnames <- purrr::map(seq(dimensions), finalnames_function) %>%
    append(list(metrics))

  #based on given names, create the list to be used for filtering and defining
  itemidnamesfunction <- function(items) {
    paste0('itemId_', dimensions[[items]])
  }
  itemidnames <-purrr::map(seq(dimensions), itemidnamesfunction)

  ##set the timeframe for the query (timeframe)
  timeframe <- make_timeframe(date_range[[1]], date_range[[2]])

##setup the right number of limits for each dimension (top)
top <- top_daterange_number(top, dimensions, date_range)

  #estimated runtime
  if(length(top) > 1) {
    toplength <- length(top)
    i <- 1
    product = 1
    topestimate1 <-  top[-toplength]
    topestimate1 <- append(topestimate1, 1, after = 0)
    for(i in seq(toplength)) {
      product <- product + prod(topestimate1[1:i])
    }
    #sec
    est_secs <- round((product-1)*.80, digits = 2)
    #min
    est_mins <- round(((product-1)*.80)/60, digits = 2)
    message(paste0('Estimated runtime: ', est_secs, 'sec./', est_mins, 'min.'))
    #message(paste0('Estimating a total of ', product-2, ' API calls'))
  }
  #segment filter builder function (segments)
  seg <- function(segmentId) {
    structure(list(type = "segment",
                   segmentId = segmentId))
  }

  segments <- purrr::map(segmentId, seg)

  #create the DateRange list item (dr)
  dr <- list(list(
    type = "dateRange",
    dateRange = timeframe))

  #join Segment and DateRange builder function
  s_dr <- function() {
    if(is.na(segmentId[[1]])) {
      list(list(
        type = "dateRange",
        dateRange = timeframe
      ))
    } else {
      append(segments, dr)
    }
  }

  #Create the global filters (gf)
  gf <- s_dr()

  #search item builder filter (search)
  search[search == ''] <- NA
  add_si <- length(dimensions) - length(search)
  added_si <- rep(NA, add_si)
  search <- append(search, added_si)

  si_fun <- function(si) {
    if(!is.na(search[si])){
      search <-  structure(list('clause' = search[si]))
    } else {
      NA
    }
  }
  search <- purrr::map(seq(dimensions), si_fun)


  ##function to create the top level 'metricsContainer'
  metriccontainer_1 <- function(metric, colId, metricSort) {
    if(colId == 0) {
      if(grepl('cm[1-9]*_*', metric)) {
        structure(list(
          columnId = colId,
          id = metric,
          sort = metricSort
        ))
      } else {
        structure(list(
          columnId = colId,
          id = sprintf('metrics/%s',metric),
          sort = metricSort
        ))
      }
    } else {
      if(grepl('cm[1-9]*_*', metric)) {
        structure(list(
          columnId = colId,
          id = metric,
          sort = metricSort
        ))
      } else {
        structure(list(
          columnId = colId,
          id = sprintf('metrics/%s',metric)
        ))
      }}
  }

  ### function to create the  breakdown 'metricsContainer'
  metriccontainer_2 <- function(metric, colId, metricSort, filterId) {
    if(colId == 0) {
      if(grepl('cm[1-9]*_*', metric)) {
        structure(list(
          columnId = colId,
          id = metric,
          sort = metricSort,
          filters = list(
            filterId
          )))
      } else {
        structure(list(
          columnId = colId,
          id = sprintf('metrics/%s',metric),
          sort = metricSort,
          filters = list(
            filterId
          )))
      }
    } else {
      if(grepl('cm[1-9]*_*', metric)) {
        structure(list(
          columnId = colId,
          id = metric,
          filters = list(
            filterId
          )))
      } else {
        structure(list(
          columnId = colId,
          id = sprintf('metrics/%s',metric),
          filters = list(
            filterId
          )))
      }}
  }

  metriccontainer_n <- function(metric, colId, metricSort , filterId) {
    if(colId == 0) {
      if(grepl('cm[1-9]*_*', metric)) {
        structure(list(
          columnId = colId,
          id = metric,
          sort = metricSort,
          filters =
            filterId
        ))
      } else {
        structure(list(
          columnId = colId,
          id = sprintf('metrics/%s',metric),
          sort = metricSort,
          filters =
            filterId
        ))
      }
    } else {
      if(grepl('cm[1-9]*_*', metric)) {
        structure(list(
          columnId = colId,
          id = metric,
          filters =
            filterId
        ))
      } else {
        structure(list(
          columnId = colId,
          id = sprintf('metrics/%s',metric),
          filters =
            filterId
        ))
      }
    }}

  #setup the tibble for building the queries
  metIds <- tibble(metrics,colid = seq(length(metrics))-1)

  df <- tibble(dimension = c(dimensions), metric = list(metIds), filterType, top)
  df <- df %>% dplyr::mutate(breakdownorder = as.numeric(rownames(df)))
  bdnumber <- as.numeric(max(df$breakdownorder))
  metnumber <- as.numeric(length(metrics))

  #metrics list items
  #if = 1st dimension
  metricContainerFunction <- function(i){
    mc <- list()
    if(i == 1) {
      mc <- list()
      m1list <- list(metric = df$metric[[i]][[1]],
                     colId = df$metric[[i]][[2]],
                     metricSort = metricSort)
      mc <- purrr::pmap(m1list, metriccontainer_1)
      return(mc)
    } else if(i == 2) {
      m2list <- list(metric = df$metric[[i]][[1]],
                     colId = df$metric[[i]][[2]],
                     metricSort = metricSort,
                     filterId = seq(nrow(df$metric[[i]][1])*(i-1))-1)
      mc <- append(mc, values = purrr::pmap(m2list, metriccontainer_2))
      return(mc)
    } else  {
      #if = 3rd dimension or more
      L <- list(seq(nrow(df$metric[[i]][1])*(i-1))-1)
      filteridslist <- split(L[[1]], rep(1:nrow(df$metric[[i]][1]), length = length(L[[1]])))

      m3list <- list(metric = df$metric[[i]][[1]],
                     colId = df$metric[[i]][[2]],
                     metricSort = metricSort,
                     filterId = filteridslist)
      mc <-  append(mc, values = purrr::pmap(m3list, metriccontainer_n))
      return(mc)
    }
  }

  mlist <- purrr::map(seq(bdnumber), metricContainerFunction)

  #function to pre-create the MetricFilters list needed to iterate through the api calls
  load_dims <- function(dimItems) {
    mflist <- list(dimension = rep(dimensions[1:dimItems], each = metnumber))
    mflist <- append(mflist, values = list('type' = 'breakdown'))
    mflist <- append(mflist, values = list('id' = seq(length(mflist[[1]]))-1))
  }
  #run the function
  mfdims <- purrr::map(seq_along(dimensions)-1, load_dims)

  #Formats a ist of metricFilters to run below the metricsContainer
  metricFiltersFunction <- function(i) {
    mfdimslist <-structure(list(id = mfdims[[i]]$id, type = 'breakdown', dimension = mfdims[[i]]$dimension, itemId = ''))
  }
  #map the function to list out the metricFiltres section of the api call
  lists_built <- purrr::map( seq_along(dimensions), metricFiltersFunction)

# 1 Call ------------------------------------------------------------------
for(i in seq(dimensions)) {
    if(i == 1) {
      #generating the body of the first api request
      req_body <- structure(
        list(
          rsid = rsid,
          globalFilters =
            gf,
          metricContainer = list(metrics = mlist[[i]]),
          dimension = sprintf("variables/%s", df$dimension[[i]]),
          search = search[[i]],
          settings = list(
            countRepeatInstances = TRUE,
            limit = top[i],
            page = page,
            nonesBehavior = unspecified
          ),
          statistics = list(functions = c("col-max", "col-min"))
        )
      )

      if (debug == FALSE) {
        res <- aw_call_data("reports/ranked", body = req_body, company_id = company_id)
      }
      if (debug == TRUE) {
        res <- aw_call_data_debug("reports/ranked", body = req_body, company_id = company_id)
      }

      resrows <- jsonlite::fromJSON(res)

      #conditional statement to determine if the function should terminate or start over with the new itemIds
      if (length(dimensions) == 1) {
        itemidname <- paste0('itemId_', dimensions[[i]])
        dat <- resrows$rows %>%
          dplyr::rename(!!itemidname := itemId,!!finalnames[[i]] := value) %>%
          dplyr::mutate(metrics = list(prefinalnames[[i + 1]])) %>%
          tidyr::unnest(c(metrics, data)) %>%
          tidyr::spread(metrics, data) %>%
          dplyr::select(all_of(finalnames))
        if(metricSort == 'desc') {
          dat <- dplyr::arrange(dat, across(starts_with(metrics), desc))
        }
        if(metricSort == 'asc') {
          dat <- dplyr::arrange(dat, across(starts_with(metrics)))
        }
        #change time variables from character strings
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

        if (prettynames == T) {
          names(dat) <- prettyfinalnames
        }
        message(paste0('A total of ',nrow(dat), ' rows have been pulled.'))

        return(dat)
      } else if (length(dimensions) != i) {
        ## second and not last data pull
        itemidname <- paste0('itemId_', dimensions[[i]])
        dat <- resrows$rows %>%
          dplyr::select(itemId, value) %>%
          dplyr::rename(!!itemidname := itemId,!!finalnames[[i]] := value)
        message(paste0('1 of ', product-1, ' possible data requests complete. Starting the next ', nrow(dat) ,' requests.'))
      }
    }

# 2 Call -----------------------------------------------------------------
  if (i == 2) {
    ### function to create the breakdown 'metricsFilters'
    metricfilter_2 <-
      function(filterId , type, dimension, itemId = '') {
        list(
          id = filterId,
          type = type,
          dimension = sprintf('variables/%s', dimension),
          itemId = itemId
        )
      }
    #run the list function to genereate the formated json string like list
    mflist <-
      list(lists_built[[i]]$id, lists_built[[i]]$type, lists_built[[i]]$dimension)

    mf_item <- purrr::pmap(mflist, metricfilter_2)

    mf_itemlist <- function(itemid) {
      purrr::map(mf_item, update_list, itemId = itemid)
    }

    api2 <- purrr::map(dat[[1]], mf_itemlist)

    req_bodies_2 <- function(i, mf = api2) {
      structure(
        list(
          rsid = rsid,
          globalFilters =
            gf,
          metricContainer = list(metrics = mlist[[i]]
                                 ,
                                 metricFilters =
                                   mf),
          dimension = sprintf("variables/%s", df$dimension[[i]]),
          search = search[[i]],
          settings = list(
            countRepeatInstances = TRUE,
            limit = top[i],
            page = page,
            nonesBehavior = unspecified
          ),
          statistics = list(functions = c("col-max", "col-min"))
        )
      )
    }

    calls <- purrr::map2(i, api2, req_bodies_2)

    call_data_n <- function(calls) {
      aw_call_data("reports/ranked", body = calls, company_id = company_id)
    }
    call_data_n_debug <- function(calls) {
      aw_call_data_debug("reports/ranked", body = calls, company_id = company_id)
    }

    if (debug == FALSE) {
      res <- purrr::map(calls, call_data_n)
    }
    if (debug == TRUE) {
      res <- purrr::map(calls, call_data_n_debug)
    }
    getdata <- function(it) {
      jsonlite::fromJSON(res[[it]])
    }

    res <- purrr::map(seq(length(res)),  getdata)

    t = 0
    el <- function(els) {
      if_else(res[[els]]$numberOfElements != 0, t + 1, 0)
    }
    elnum <- sum(unlist(purrr::map(seq(length(
      res
    )), el)))

    rowsdata <- function(it, i) {
      if (res[[it]]$numberOfElements != 0) {
        res[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[1]][[1]] := dat[[1]][[it]],!!prefinalnames[[1]][[2]] := dat[[2]][[it]])
      }
    }

    resrows <- purrr::map2_dfr(seq(elnum), i, rowsdata)

    #conditional statement to determine if the function should terminate or rerun the next iteration of api calls
    if (length(dimensions) != i) {
      ## second and not last data pull
      itemidname <- paste0('itemId_', dimensions[[i]])
      dat <- resrows %>%
        dplyr::rename(!!itemidname := itemId,!!finalnames[[i]] := value)
      dat <- dat %>% dplyr::select(-data)
      message(paste0('Starting the next ', nrow(dat) ,' requests.'))
    } else {
      itemidname <- paste0('itemId_', dimensions[[i]])
      dat <- resrows %>%
        dplyr::rename(!!itemidname := itemId,!!finalnames[[i]] := value) %>%
        dplyr::mutate(metrics = list(prefinalnames[[i + 1]])) %>%
        tidyr::unnest(c(metrics, data)) %>%
        tidyr::spread(metrics, data) %>%
        dplyr::select(all_of(finalnames))
        if(metricSort == 'desc') {
          dat <- dplyr::arrange(dat, across(starts_with(metrics), desc))
        }
      if(metricSort == 'asc') {
         dat <- dplyr::arrange(dat, across(starts_with(metrics)))
      }
      #change time variables from character strings
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

      if (prettynames == T) {
        names(dat) <- prettyfinalnames
      }
      message(paste0('A total of ',nrow(dat), ' rows have been pulled.'))
      return(dat)
    }
  }

# N Calls -----------------------------------------------------------------
  if(i >= 3 && i <= length(dimensions)) {

      # a function that formats the list of metricFilters too run below the metricsContainer
      metricFiltersFunction <- function(i) {
        mfdimslist <-structure(list(id = mfdims[[i]]$id, type = 'breakdown', dimension = mfdims[[i]]$dimension))
      }
      #map the function to list out the metricFiltres section of the api call
      lists_built <- purrr::map( seq_along(dimensions), metricFiltersFunction)

      ### function to create the breakdown 'metricsFilters'
      metricfilter_n <- function(filterId , type, dimension) {
        list(
          id = filterId,
          type = type,
          dimension = sprintf('variables/%s', dimension)
        )
      }

      #run the list function to genereate the formated json string like list
      mflist <- list(lists_built[[i]]$id, lists_built[[i]]$type, lists_built[[i]]$dimension)

      #pulls together all the main items minus the itemIds for the query
      mf_item <- purrr::pmap(mflist, metricfilter_n)

      #build the item ids needed for the next query
      mf_itemlist <- function(itemid) {
        ids <- purrr::map(purrr::map_depth(itemid, 1, unlist), rep,  each = length(metrics))
      }

      selectlist <- list()
      for(series in seq(i-1)){
        selectlist <-  append(selectlist, itemidnames[[series]])
      }
      itemidlist_n <- dplyr::select(dat, unlist(selectlist))

      listum <- list()

      for(n_item in seq(nrow(itemidlist_n))) {
        listum <- append(listum, list(paste(itemidlist_n[n_item, ])))
      }
      itemidlist_n <- listum

      ##Create the itemids list in the correct number of times.
      itemidser <- purrr::map(itemidlist_n, mf_itemlist)

      ##join the 2 different itemids in their correct order. (ncapable)
      listing <- function(p = seq(itemidser)) {
        unlist(itemidser[[p]], use.names = F)
      }

      ##creating the list of lists for the appropriate number of metricFilter items (ncapable)
      itemidser <- purrr::map(seq(itemidser),  listing)

      #duplicate the list to match the list length of the next api call (ncapable)
      mf_list <- rep(list(mf_item), length(itemidser))

      #create the list that will hold the list of api calls (ncapable)
      apicalls <- rep(list(rep(list(), length(mf_list[[1]]))), length(mf_list))

      for(l in seq(mf_list)) {
        for(t in seq(mf_list[[1]])) {
          apicalls[[l]][[t]] <- append(mf_list[[l]][[t]],  list('itemId'=itemidser[[l]][t]))
        }
      }

      #(ncapable)
      req_bodies <- function(i, mf = apicalls) {
        structure(list(rsid = rsid,
                       globalFilters =
                         gf,
                       metricContainer = list(
                         metrics = mlist[[i]]
                         ,
                         metricFilters =
                           mf
                       ),
                       dimension = sprintf("variables/%s",df$dimension[[i]]),
                       search = search[[i]],
                       settings = list(
                         countRepeatInstances = TRUE,
                         limit = top[i],
                         page = page,
                         nonesBehavior = unspecified
                       ),
                       statistics = list(
                         functions = c("col-max", "col-min")
                       ) ) )
      }

      #(ncapable)
      calls <- purrr::map2(i, apicalls, req_bodies)

      #(ncapable)
      call_data_n <- function(calls) {
        aw_call_data("reports/ranked", body = calls, company_id = company_id)
      }
      call_data_n_debug <- function(calls) {
        aw_call_data_debug("reports/ranked", body = calls, company_id = company_id)
      }

      #(ncapable)
      if(debug == FALSE) {
        res <- purrr::map(calls, call_data_n)
      }
      if(debug == TRUE) {
        res <- purrr::map(calls, call_data_n_debug)
      }

      #(ncapable)
      getdata <- function(it) {
        jsonlite::fromJSON(res[[it]])
      }

      #(ncapable)
      resn <- purrr::map(seq(length(res)),  getdata)


      #apply previous rows of data to the most recent data request
      resrows <- apply_rowsdata(i, resn, prefinalnames, dat)

      if(length(dimensions) != i) {
        ## second and not last data pull
        itemidname <- paste0('itemId_', finalnames[[i]])
        dat <- resrows %>%
          dplyr::rename(!!itemidname := itemId,
                 !!finalnames[[i]] := value)
        dat <- dat %>% dplyr::select(-data)
        message(paste0('Starting the next ', nrow(dat) ,' requests.'))
      } else {
        itemidname <- paste0('itemId_', dimensions[[i]])
        dat <- resrows %>%
          dplyr::rename(!!itemidname := itemId,
                 !!finalnames[[i]] := value) %>%
          dplyr::mutate(metrics = list(prefinalnames[[i+1]])) %>%
          tidyr::unnest(c(metrics, data)) %>%
          tidyr::spread(metrics, data) %>%
          dplyr::select(all_of(finalnames))
        if(metricSort == 'desc') {
          dat <- dplyr::arrange(dat, across(starts_with(metrics), desc))
        }
        if(metricSort == 'asc') {
          dat <- dplyr::arrange(dat, across(starts_with(metrics)))
        }
        #change time variables from character strings
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

        if(prettynames == T) {
          names(dat) <- prettyfinalnames
        }
        message(paste0('A total of ',nrow(dat), ' rows have been pulled.'))
        return(dat)
      }
   }
  }
}


