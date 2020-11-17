#' Get a free form report
#'
#' Organizes the arguments into a json string and then structures the data after the internal function makes
#' the api call. Up to 15 dimensions at this time.
#'
#' @param company_id Company Id.  Taken from the global environment by default if not provided.
#' @param rsid Adobe report suite id number. Taken from the global environment by default if not provided.
#' @param date_range A two length vector of start and end Date objects
#' @param metrics A character vector of metrics.
#' @param dimensions A character vector of dimensions. There is a limit of 15 at this time.
#' @param top How many rows. Default is set to 5. If using 'daterangeday' as the first variable you can either use only one number item (top = 5) or you can add a 0 as the first in the list of numbers (top = c(0, 20)). The function will then calculate how many days are included for you. This only works if daterangeday is the first dimension listed.
#' @param metricSort Presorts the table by metrics. Values are either 'asc' or 'desc'.
#' @param filterType Currently, Only 'breakdown' is supported but future versions should include 'segment' breakdown capability.
#' @param include_unspecified TRUE is equal to "return-nones" and is set as the default
#' @param segmentId use segments to globally filter the results. Use 1 or many.
#' @param search The structure of a search string is specific and can be compound using "AND". Each dimension can be filtered using a unique item in a character vector item.
#' @param prettynames Boolean for whether the results should have the name field value for easier understanding.
#' @param debug default is FALSE but set to TRUE to see the json request being sent to the Adobe API
#'
#' @return Data Frame
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
aa_freeform_report <- function(company_id = Sys.getenv("AA_COMPANY_ID"),
                               rsid = Sys.getenv("AA_REPORTSUITE_ID"),
                               date_range = c(Sys.Date()-30, Sys.Date()-1),
                               dimensions = c('page', 'lasttouchchannel', 'mobiledevicetype'),
                               metrics = c("visits", "visitors"),
                               top = c(5),
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
  dims <- aa_get_dimensions(rsid = rsid, company_id = company_id)
  mets <- aa_get_metrics(rsid = rsid, company_id = company_id)
  #pull out the calculated metrics
  cms_ids <- metrics[grepl('cm[1-9]*_*', metrics)]
  if (length(cms_ids) > 0) {
    cms <- aa_get_calculatedmetrics(company_id = company_id, filterByIds = cms_ids)
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
  if(length(top) != length(dimensions) & length(top) != 1) {
    stop('TOP length: The "top" number of values must be equal the length of the "dimensions" list or 1 unless the first dimension is a "daterange" metric in which case the number of "top" items only has to match the length of the non "daterange" items.')
  } else if(grepl('daterangeday', dimensions[1]) & length(top) == 1) {
    top <- rep(top, length(dimensions)-1)
    top <- c(as.numeric(as.Date(date_range[2]) - as.Date(date_range[[1]])+1), top)
  } else if(grepl('daterangeday', dimensions[1]) & length(top) != 1 & top[[1]] == 0) {
    top[[1]] <- as.numeric(as.Date(date_range[2]) - as.Date(date_range[[1]])+1)
  } else if(length(top) == 1) {
    top <- rep(top, length(dimensions))
  }

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
    message(paste0('Estimating a total of ', product-2, ' API calls'))
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
  metriccontainer_1 <- function(metric, colId, metricSort = 'desc') {
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
  metriccontainer_2 <- function(metric, colId, metricSort = 'desc' , filterId) {
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

  metriccontainer_n <- function(metric, colId, metricSort = 'desc' , filterId) {
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
      mc <- purrr::map2(df$metric[[i]][[1]], df$metric[[i]][[2]], metriccontainer_1)
      return(mc)
    } else if(i == 2) {
      m2list <- list(metric = df$metric[[i]][[1]], colId = df$metric[[i]][[2]],
                     metricSort = metricSort, filterId = seq(nrow(df$metric[[i]][1])*(i-1))-1)
      mc <- append(mc, values = purrr::pmap(m2list, metriccontainer_2))
      return(mc)
    } else  {
      #if = 3rd dimension or more
      L <- list(seq(nrow(df$metric[[i]][1])*(i-1))-1)
      filteridslist <- split(L[[1]], rep(1:nrow(df$metric[[i]][1]), length = length(L[[1]])))

      m3list <- list(metric = df$metric[[i]][[1]],
                     colId = df$metric[[i]][[2]],metricSort = metricSort,
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
            page = 0,
            nonesBehavior = unspecified
          ),
          statistics = list(functions = c("col-max", "col-min"))
        )
      )

      if (debug == FALSE) {
        res <-
          aa_call_data("reports/ranked", body = req_body, company_id = company_id)
      }
      if (debug == TRUE) {
        res <-
          aa_call_data_debug("reports/ranked", body = req_body, company_id = company_id)
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
        message(paste0(nrow(dat), ' API calls to go.'))
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
            page = 0,
            nonesBehavior = unspecified
          ),
          statistics = list(functions = c("col-max", "col-min"))
        )
      )
    }

    calls <- purrr::map2(i, api2, req_bodies_2)

    call_data_n <- function(calls) {
      aa_call_data("reports/ranked", body = calls, company_id = company_id)
    }
    call_data_n_debug <- function(calls) {
      aa_call_data_debug("reports/ranked", body = calls, company_id = company_id)
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
      message(paste0(nrow(dat), ' more API calls to go.'))
    } else {
      itemidname <- paste0('itemId_', dimensions[[i]])
      dat <- resrows %>%
        dplyr::rename(!!itemidname := itemId,!!finalnames[[i]] := value) %>%
        dplyr::mutate(metrics = list(prefinalnames[[i + 1]])) %>%
        tidyr::unnest(c(metrics, data)) %>%
        tidyr::spread(metrics, data) %>%
        dplyr::select(all_of(finalnames))
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
                         page = 0,
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
          aa_call_data("reports/ranked", body = calls, company_id = company_id)
      }
      call_data_n_debug <- function(calls) {
          aa_call_data_debug("reports/ranked", body = calls, company_id = company_id)
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

    rowsdata <- function(it, i) {
        if(i == 2) {
          if(res[[it]]$numberOfElements != 0) {
            tf <- res[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[1]][[1]] := dat[[1]][[it]],
                                          !!prefinalnames[[1]][[2]] := dat[[2]][[it]])
          return(tf)
          }
        }
        if(i == 3) {
          if(resn[[it]]$numberOfElements != 0) {
            tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[2]][[1]] := dat[[1]][it],
                                           !!prefinalnames[[2]][[2]] := dat[[2]][it],
                                           !!prefinalnames[[1]][[1]] := dat[[3]][it],
                                           !!prefinalnames[[1]][[2]] := dat[[4]][it])

            return(tf)
          }
        }
        if(i == 4) {
          if(resn[[it]]$numberOfElements != 0) {
            tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[3]][[1]] := dat[[1]][it],
                                           !!prefinalnames[[3]][[2]] := dat[[2]][it],
                                           !!prefinalnames[[2]][[1]] := dat[[3]][it],
                                           !!prefinalnames[[2]][[2]] := dat[[4]][it],
                                           !!prefinalnames[[1]][[1]] := dat[[5]][it],
                                           !!prefinalnames[[1]][[2]] := dat[[6]][it])
            return(tf)
          }
        }
        if(i == 5) {
          if(resn[[it]]$numberOfElements != 0) {
            tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[4]][[1]] := dat[[1]][it],
                                           !!prefinalnames[[4]][[2]] := dat[[2]][it],
                                           !!prefinalnames[[3]][[1]] := dat[[3]][it],
                                           !!prefinalnames[[3]][[2]] := dat[[4]][it],
                                           !!prefinalnames[[2]][[1]] := dat[[5]][it],
                                           !!prefinalnames[[2]][[2]] := dat[[6]][it],
                                           !!prefinalnames[[1]][[1]] := dat[[7]][it],
                                           !!prefinalnames[[1]][[2]] := dat[[8]][it])
            return(tf)
          }
        }
        if(i == 6) {
          if(resn[[it]]$numberOfElements != 0) {
            tf <-  resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[5]][[1]] := dat[[1]][it],
                                            !!prefinalnames[[5]][[2]] := dat[[2]][it],
                                            !!prefinalnames[[4]][[1]] := dat[[3]][it],
                                            !!prefinalnames[[4]][[2]] := dat[[4]][it],
                                            !!prefinalnames[[3]][[1]] := dat[[5]][it],
                                            !!prefinalnames[[3]][[2]] := dat[[6]][it],
                                            !!prefinalnames[[2]][[1]] := dat[[7]][it],
                                            !!prefinalnames[[2]][[2]] := dat[[8]][it],
                                            !!prefinalnames[[1]][[1]] := dat[[9]][it],
                                            !!prefinalnames[[1]][[2]] := dat[[10]][it])
            return(tf)
          }
        }
        if(i == 7) {
          if(resn[[it]]$numberOfElements != 0) {
            tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[6]][[1]] := dat[[1]][it],
                                           !!prefinalnames[[6]][[2]] := dat[[2]][it],
                                           !!prefinalnames[[5]][[1]] := dat[[3]][it],
                                           !!prefinalnames[[5]][[2]] := dat[[4]][it],
                                           !!prefinalnames[[4]][[1]] := dat[[5]][it],
                                           !!prefinalnames[[4]][[2]] := dat[[6]][it],
                                           !!prefinalnames[[3]][[1]] := dat[[7]][it],
                                           !!prefinalnames[[3]][[2]] := dat[[8]][it],
                                           !!prefinalnames[[2]][[1]] := dat[[9]][it],
                                           !!prefinalnames[[2]][[2]] := dat[[10]][it],
                                           !!prefinalnames[[1]][[1]] := dat[[11]][it],
                                           !!prefinalnames[[1]][[2]] := dat[[12]][it])
            return(tf)
          }
        }
        if(i == 8) {
          if(resn[[it]]$numberOfElements != 0) {
           tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[7]][[1]] := dat[[1]][it],
                                           !!prefinalnames[[7]][[2]] := dat[[2]][it],
                                           !!prefinalnames[[6]][[1]] := dat[[3]][it],
                                           !!prefinalnames[[6]][[2]] := dat[[4]][it],
                                           !!prefinalnames[[5]][[1]] := dat[[5]][it],
                                           !!prefinalnames[[5]][[2]] := dat[[6]][it],
                                           !!prefinalnames[[4]][[1]] := dat[[7]][it],
                                           !!prefinalnames[[4]][[2]] := dat[[8]][it],
                                           !!prefinalnames[[3]][[1]] := dat[[9]][it],
                                           !!prefinalnames[[3]][[2]] := dat[[10]][it],
                                           !!prefinalnames[[2]][[1]] := dat[[11]][it],
                                           !!prefinalnames[[2]][[2]] := dat[[12]][it],
                                           !!prefinalnames[[1]][[1]] := dat[[13]][it],
                                           !!prefinalnames[[1]][[2]] := dat[[14]][it])
           return(tf)
          }
        }
        if(i == 9) {
          if(resn[[it]]$numberOfElements != 0) {
           tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[8]][[1]] := dat[[1]][it],
                                           !!prefinalnames[[8]][[2]] := dat[[2]][it],
                                           !!prefinalnames[[7]][[1]] := dat[[3]][it],
                                           !!prefinalnames[[7]][[2]] := dat[[4]][it],
                                           !!prefinalnames[[6]][[1]] := dat[[5]][it],
                                           !!prefinalnames[[6]][[2]] := dat[[6]][it],
                                           !!prefinalnames[[5]][[1]] := dat[[7]][it],
                                           !!prefinalnames[[5]][[2]] := dat[[8]][it],
                                           !!prefinalnames[[4]][[1]] := dat[[9]][it],
                                           !!prefinalnames[[4]][[2]] := dat[[10]][it],
                                           !!prefinalnames[[3]][[1]] := dat[[11]][it],
                                           !!prefinalnames[[3]][[2]] := dat[[12]][it],
                                           !!prefinalnames[[2]][[1]] := dat[[13]][it],
                                           !!prefinalnames[[2]][[2]] := dat[[14]][it],
                                           !!prefinalnames[[1]][[1]] := dat[[15]][it],
                                           !!prefinalnames[[1]][[2]] := dat[[16]][it])
           return(tf)
          }
        }
        if(i == 10) {
          if(resn[[it]]$numberOfElements != 0) {
            tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[9]][[1]] := dat[[1]][it],
                                           !!prefinalnames[[9]][[2]] := dat[[2]][it],
                                           !!prefinalnames[[8]][[1]] := dat[[3]][it],
                                           !!prefinalnames[[8]][[2]] := dat[[4]][it],
                                           !!prefinalnames[[7]][[1]] := dat[[5]][it],
                                           !!prefinalnames[[7]][[2]] := dat[[6]][it],
                                           !!prefinalnames[[6]][[1]] := dat[[7]][it],
                                           !!prefinalnames[[6]][[2]] := dat[[8]][it],
                                           !!prefinalnames[[5]][[1]] := dat[[9]][it],
                                           !!prefinalnames[[5]][[2]] := dat[[10]][it],
                                           !!prefinalnames[[4]][[1]] := dat[[11]][it],
                                           !!prefinalnames[[4]][[2]] := dat[[12]][it],
                                           !!prefinalnames[[3]][[1]] := dat[[13]][it],
                                           !!prefinalnames[[3]][[2]] := dat[[14]][it],
                                           !!prefinalnames[[2]][[1]] := dat[[15]][it],
                                           !!prefinalnames[[2]][[2]] := dat[[16]][it],
                                           !!prefinalnames[[1]][[1]] := dat[[17]][it],
                                           !!prefinalnames[[1]][[2]] := dat[[18]][it])
            return(tf)
          }
        }
        if(i == 11) {
          if(resn[[it]]$numberOfElements != 0) {
           tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[10]][[1]] := dat[[1]][it],
                                                  !!prefinalnames[[10]][[2]] := dat[[2]][it],
                                                  !!prefinalnames[[9]][[1]] := dat[[3]][it],
                                                  !!prefinalnames[[9]][[2]] := dat[[4]][it],
                                                  !!prefinalnames[[8]][[1]] := dat[[5]][it],
                                                  !!prefinalnames[[8]][[2]] := dat[[6]][it],
                                                  !!prefinalnames[[7]][[1]] := dat[[7]][it],
                                                  !!prefinalnames[[7]][[2]] := dat[[8]][it],
                                                  !!prefinalnames[[6]][[1]] := dat[[9]][it],
                                                  !!prefinalnames[[6]][[2]] := dat[[10]][it],
                                                  !!prefinalnames[[5]][[1]] := dat[[11]][it],
                                                  !!prefinalnames[[5]][[2]] := dat[[12]][it],
                                                  !!prefinalnames[[4]][[1]] := dat[[13]][it],
                                                  !!prefinalnames[[4]][[2]] := dat[[14]][it],
                                                  !!prefinalnames[[3]][[1]] := dat[[15]][it],
                                                  !!prefinalnames[[3]][[2]] := dat[[16]][it],
                                                  !!prefinalnames[[2]][[1]] := dat[[17]][it],
                                                  !!prefinalnames[[2]][[2]] := dat[[18]][it],
                                                  !!prefinalnames[[1]][[1]] := dat[[19]][it],
                                                  !!prefinalnames[[1]][[2]] := dat[[20]][it])
           return(tf)
          }
        }
        if(i == 12) {
          if(resn[[it]]$numberOfElements != 0) {
           tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[11]][[1]] := dat[[1]][it],
                                                  !!prefinalnames[[11]][[2]] := dat[[2]][it],
                                                  !!prefinalnames[[10]][[1]] := dat[[3]][it],
                                                  !!prefinalnames[[10]][[2]] := dat[[4]][it],
                                                  !!prefinalnames[[9]][[1]] := dat[[5]][it],
                                                  !!prefinalnames[[9]][[2]] := dat[[6]][it],
                                                  !!prefinalnames[[8]][[1]] := dat[[7]][it],
                                                  !!prefinalnames[[8]][[2]] := dat[[8]][it],
                                                  !!prefinalnames[[7]][[1]] := dat[[9]][it],
                                                  !!prefinalnames[[7]][[2]] := dat[[10]][it],
                                                  !!prefinalnames[[6]][[1]] := dat[[11]][it],
                                                  !!prefinalnames[[6]][[2]] := dat[[12]][it],
                                                  !!prefinalnames[[5]][[1]] := dat[[13]][it],
                                                  !!prefinalnames[[5]][[2]] := dat[[14]][it],
                                                  !!prefinalnames[[4]][[1]] := dat[[15]][it],
                                                  !!prefinalnames[[4]][[2]] := dat[[16]][it],
                                                  !!prefinalnames[[3]][[1]] := dat[[17]][it],
                                                  !!prefinalnames[[3]][[2]] := dat[[18]][it],
                                                  !!prefinalnames[[2]][[1]] := dat[[19]][it],
                                                  !!prefinalnames[[2]][[2]] := dat[[20]][it],
                                                  !!prefinalnames[[1]][[1]] := dat[[21]][it],
                                                  !!prefinalnames[[1]][[2]] := dat[[22]][it])
           return(tf)
          }
        }
        if(i == 13) {
          if(resn[[it]]$numberOfElements != 0) {
            tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[12]][[1]] := dat[[1]][it],
                                                  !!prefinalnames[[12]][[2]] := dat[[2]][it],
                                                  !!prefinalnames[[11]][[1]] := dat[[3]][it],
                                                  !!prefinalnames[[11]][[2]] := dat[[4]][it],
                                                  !!prefinalnames[[10]][[1]] := dat[[5]][it],
                                                  !!prefinalnames[[10]][[2]] := dat[[6]][it],
                                                  !!prefinalnames[[9]][[1]] := dat[[7]][it],
                                                  !!prefinalnames[[9]][[2]] := dat[[8]][it],
                                                  !!prefinalnames[[8]][[1]] := dat[[9]][it],
                                                  !!prefinalnames[[8]][[2]] := dat[[10]][it],
                                                  !!prefinalnames[[7]][[1]] := dat[[11]][it],
                                                  !!prefinalnames[[7]][[2]] := dat[[12]][it],
                                                  !!prefinalnames[[6]][[1]] := dat[[13]][it],
                                                  !!prefinalnames[[6]][[2]] := dat[[14]][it],
                                                  !!prefinalnames[[5]][[1]] := dat[[15]][it],
                                                  !!prefinalnames[[5]][[2]] := dat[[16]][it],
                                                  !!prefinalnames[[4]][[1]] := dat[[17]][it],
                                                  !!prefinalnames[[4]][[2]] := dat[[18]][it],
                                                  !!prefinalnames[[3]][[1]] := dat[[19]][it],
                                                  !!prefinalnames[[3]][[2]] := dat[[20]][it],
                                                  !!prefinalnames[[2]][[1]] := dat[[21]][it],
                                                  !!prefinalnames[[2]][[2]] := dat[[22]][it],
                                                  !!prefinalnames[[1]][[1]] := dat[[23]][it],
                                                  !!prefinalnames[[1]][[2]] := dat[[24]][it])
            return(tf)
          }
        }
        if(i == 14) {
          if(resn[[it]]$numberOfElements != 0) {
            tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[13]][[1]] := dat[[1]][it],
                                                  !!prefinalnames[[13]][[2]] := dat[[2]][it],
                                                  !!prefinalnames[[12]][[1]] := dat[[3]][it],
                                                  !!prefinalnames[[12]][[2]] := dat[[4]][it],
                                                  !!prefinalnames[[11]][[1]] := dat[[5]][it],
                                                  !!prefinalnames[[11]][[2]] := dat[[6]][it],
                                                  !!prefinalnames[[10]][[1]] := dat[[7]][it],
                                                  !!prefinalnames[[10]][[2]] := dat[[8]][it],
                                                  !!prefinalnames[[9]][[1]] := dat[[9]][it],
                                                  !!prefinalnames[[9]][[2]] := dat[[10]][it],
                                                  !!prefinalnames[[8]][[1]] := dat[[11]][it],
                                                  !!prefinalnames[[8]][[2]] := dat[[12]][it],
                                                  !!prefinalnames[[7]][[1]] := dat[[13]][it],
                                                  !!prefinalnames[[7]][[2]] := dat[[14]][it],
                                                  !!prefinalnames[[6]][[1]] := dat[[15]][it],
                                                  !!prefinalnames[[6]][[2]] := dat[[16]][it],
                                                  !!prefinalnames[[5]][[1]] := dat[[17]][it],
                                                  !!prefinalnames[[5]][[2]] := dat[[18]][it],
                                                  !!prefinalnames[[4]][[1]] := dat[[19]][it],
                                                  !!prefinalnames[[4]][[2]] := dat[[20]][it],
                                                  !!prefinalnames[[3]][[1]] := dat[[21]][it],
                                                  !!prefinalnames[[3]][[2]] := dat[[22]][it],
                                                  !!prefinalnames[[2]][[1]] := dat[[23]][it],
                                                  !!prefinalnames[[2]][[2]] := dat[[24]][it],
                                                  !!prefinalnames[[1]][[1]] := dat[[25]][it],
                                                  !!prefinalnames[[1]][[2]] := dat[[26]][it])
            return(tf)
          }
        }
        if(i == 15) {
          if(resn[[it]]$numberOfElements != 0) {
            tf <- resn[[it]]$rows %>% dplyr::mutate(!!prefinalnames[[14]][[1]] := dat[[1]][it],
                                                  !!prefinalnames[[15]][[2]] := dat[[2]][it],
                                                  !!prefinalnames[[13]][[1]] := dat[[3]][it],
                                                  !!prefinalnames[[13]][[2]] := dat[[4]][it],
                                                  !!prefinalnames[[12]][[1]] := dat[[5]][it],
                                                  !!prefinalnames[[12]][[2]] := dat[[6]][it],
                                                  !!prefinalnames[[11]][[1]] := dat[[7]][it],
                                                  !!prefinalnames[[11]][[2]] := dat[[8]][it],
                                                  !!prefinalnames[[10]][[1]] := dat[[9]][it],
                                                  !!prefinalnames[[10]][[2]] := dat[[10]][it],
                                                  !!prefinalnames[[9]][[1]] := dat[[11]][it],
                                                  !!prefinalnames[[9]][[2]] := dat[[12]][it],
                                                  !!prefinalnames[[8]][[1]] := dat[[13]][it],
                                                  !!prefinalnames[[8]][[2]] := dat[[14]][it],
                                                  !!prefinalnames[[7]][[1]] := dat[[15]][it],
                                                  !!prefinalnames[[7]][[2]] := dat[[16]][it],
                                                  !!prefinalnames[[6]][[1]] := dat[[17]][it],
                                                  !!prefinalnames[[6]][[2]] := dat[[18]][it],
                                                  !!prefinalnames[[5]][[1]] := dat[[19]][it],
                                                  !!prefinalnames[[5]][[2]] := dat[[20]][it],
                                                  !!prefinalnames[[4]][[1]] := dat[[21]][it],
                                                  !!prefinalnames[[4]][[2]] := dat[[22]][it],
                                                  !!prefinalnames[[3]][[1]] := dat[[23]][it],
                                                  !!prefinalnames[[3]][[2]] := dat[[24]][it],
                                                  !!prefinalnames[[2]][[1]] := dat[[25]][it],
                                                  !!prefinalnames[[2]][[2]] := dat[[26]][it],
                                                  !!prefinalnames[[1]][[1]] := dat[[27]][it],
                                                  !!prefinalnames[[1]][[2]] := dat[[28]][it])
            return(tf)
          }
        }
      }

      resrows <- purrr::map2_dfr(seq(length(resn)), i, rowsdata)


      if(length(dimensions) != i) {
        ## second and not last data pull
        itemidname <- paste0('itemId_', finalnames[[i]])
        dat <- resrows %>%
          dplyr::rename(!!itemidname := itemId,
                 !!finalnames[[i]] := value)
        dat <- dat %>% dplyr::select(-data)
        message(paste0(nrow(dat), ' more API calls to go.'))

      } else {
        itemidname <- paste0('itemId_', dimensions[[i]])
        dat <- resrows %>%
          dplyr::rename(!!itemidname := itemId,
                 !!finalnames[[i]] := value) %>%
          dplyr::mutate(metrics = list(prefinalnames[[i+1]])) %>%
          tidyr::unnest(c(metrics, data)) %>%
          tidyr::spread(metrics, data) %>%
          dplyr::select(all_of(finalnames))
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


