#' Get a free form report
#'
#' Organizes the arguments into a json string and then structures the data after the internal function makes
#' the api call. Up to 7 dimensions at this time.
#'
#' @param rsid Adobe report suite id number. Taken from the global environment by default if not provided.
#' @param date_range A two length vector of start and end Date objects
#' @param metrics Metric to send
#' @param dimensions Dimension to send
#' @param top How many rows. Default is set to 5. If using 'daterangeday' as the first variable you can either use only one limi number or you can add
#' a 0 as the first in the list of numbers. The function will then calculate how many days are included for you. This only works
#' if daterangeday is the first dimension listed.
#' @param metricSort Presorts the table by metrics. Values are either 'asc' or 'desc'.
#' @param filterType Default is 'breakdown'. This will only change if a segment is used.
#' @param include_unspecified TRUE is equal to "return-nones" and is set as the default
#' @param segmentId use segments to globally filter the results. Use 1 or many.
#' @param search The structure of a search string is specific and can be compound using "and"...more to come
#' @param debug default is TRUE but set to TRUE to see the json request being sent to the Adobe API
#'
#' @return Data Frame
#'
#' @import assertthat
#' @import httr
#' @import tidyverse
#' @import jsonlite
#' @import httr
#' @import dplyr
#' @import curl
#' @import stringr
#' @import purrrlyr
#'
#' @export
aa_freeform_report <- function(company_id = Sys.getenv("AA_COMPANY_ID"),
                               rsid = Sys.getenv("AA_REPORTSUITE_ID"),
                               date_range = c("2020-08-01", "2020-09-25"),
                               dimensions = c('page', 'lasttouchchannel', 'mobiledevicetype'),
                               metrics = c("visits", "visitors"),
                               top = c(5),
                               filterType = 'breakdown',
                               segmentId = NA,
                               metricSort =  'desc',
                               include_unspecified = TRUE,
                               debug = FALSE,
                               search = NA
                               )
{

  #Identify the handling of unspecified
  if(include_unspecified == FALSE){
    unspecified <- "exclude-nones"
  }
  if(include_unspecified == TRUE) {
    unspecified <- "return-nones"
  }

  # 1 Call ------------------------------------------------------------------
for(i in seq(dimensions)) {
    if(i == 1) {
      finalnames <- c(dimensions, metrics)

      itemId <- list(dimensions)

      finalnames_function <- function(level) {
        c(paste0('itemId_',dimensions[level]), dimensions[level])
      }

      prefinalnames <- map(seq(dimensions), finalnames_function) %>%
        append(list(metrics))


      #based on given names, create the list to be used for filtering and defining
      itemidnamesfunction <- function(items) {
        paste0('itemId_', dimensions[[items]])
      }
      itemidnames <-map(seq(dimensions), itemidnamesfunction)

      ##set the timeframe for the query
      timeframe <- make_timeframe(date_range[[1]], date_range[[2]])

      ##setup the right number of limits for each dimension
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

  #segment filter builder function
      seg <- function(segmentId) {
        structure(list(type = "segment",
                       segmentId = segmentId))
      }

      segments <- map(segmentId, seg)

  #create the DateRange list item
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

      #Create the global filters
      gf <- s_dr()

      #search filter
      if(!is.na(search)){
        search <-  structure(list('clause' = search))
      }

      ##function to create the top level 'metricsContainer'
      metriccontainer_1 <- function(metric, colId, metricSort = 'desc') {
        if(colId == 0) {
          structure(list(
            columnId = colId,
            id = sprintf('metrics/%s',metric),
            sort = metricSort
          ))
        } else {
          structure(list(
            columnId = colId,
            id = sprintf('metrics/%s',metric)))
        }}

      ### function to create the  breakdown 'metricsContainer'
      metriccontainer_2 <- function(metric, colId, metricSort = 'desc' , filterId) {
        if(colId == 0) {
          structure(list(
            columnId = colId,
            id = sprintf('metrics/%s',metric),
            sort = metricSort,
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

      metriccontainer_n <- function(metric, colId, metricSort = 'desc' , filterId) {
        if(colId == 0) {
          structure(list(
            columnId = colId,
            id = sprintf('metrics/%s',metric),
            sort = metricSort,
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
        }}

      #setup the tibble for building the queries
      metIds <- tibble(metrics,colid = seq(length(metrics))-1)

      df <- tibble(dimension = c(dimensions), metric = list(metIds), filterType, top)
      df <- df %>% mutate(breakdownorder = as.numeric(rownames(df)))
      bdnumber <- as.numeric(max(df$breakdownorder))
      metnumber <- as.numeric(length(metrics))

      #metrics list items
      #if = 1st dimension
      metricContainerFunction <- function(i){
        mc <- list()
        if(i == 1) {
          mc <- list()
          mc <- map2(df$metric[[i]][[1]], df$metric[[i]][[2]], metriccontainer_1)
          return(mc)
        } else if(i == 2) {
          m2list <- list(metric = df$metric[[i]][[1]], colId = df$metric[[i]][[2]],
                         metricSort = metricSort, filterId = seq(nrow(df$metric[[i]][1])*(i-1))-1)
          mc <- append(mc, values = pmap(m2list, metriccontainer_2))
          return(mc)
        } else  {
          #if = 3rd dimension or more
          L <- list(seq(nrow(df$metric[[i]][1])*(i-1))-1)
          filteridslist <- split(L[[1]], rep(1:nrow(df$metric[[i]][1]), length = length(L[[1]])))

          m3list <- list(metric = df$metric[[i]][[1]],
                         colId = df$metric[[i]][[2]],metricSort = metricSort,
                         filterId = filteridslist)
          mc <-  append(mc, values = pmap(m3list, metriccontainer_n))
          return(mc)
        }
      }

      mlist <- map(seq(bdnumber), metricContainerFunction)


      #generating the body of the first api request
      req_body <- structure(list(rsid = rsid,
                                 globalFilters =
                                   gf,
                                 metricContainer = list(
                                   metrics = mlist[[i]]
                                 ),
                                 dimension = sprintf("variables/%s", df$dimension[[i]]),
                                 search = search,
                                 settings = list(
                                   countRepeatInstances = TRUE,
                                   limit = top[i],
                                   page = 0,
                                   nonesBehavior = unspecified
                                 ),
                                 statistics = list(
                                   functions = c("col-max", "col-min")
                                 )))

      if(debug == FALSE) {
        res <- aa_call_data("reports/ranked", body = req_body, company_id = company_id)
      }
      if(debug == TRUE) {
        res <- aa_call_data_debug("reports/ranked", body = req_body, company_id = company_id)
      }

      resrows<- fromJSON(res)


      #conditional statement to determine if the function should terminate and reurn the df or continue on.
      if(length(dimensions) == 1) {
        itemidname <- paste0('itemId_', dimensions[[i]])
        dat <- resrows$rows %>%
          rename(!!itemidname := itemId,
                 !!finalnames[[i]] := value) %>%
          mutate(metrics = list(prefinalnames[[i+1]])) %>%
          unnest(c(metrics, data)) %>%
          spread(metrics, data) %>%
          select(finalnames)
        return(dat)
      } else if(length(dimensions) != i) {
        ## second and not last data pull
        itemidname <- paste0('itemId_', dimensions[[i]])
        dat <- resrows$rows %>%
          select(itemId, value) %>%
          rename(!!itemidname := itemId,
                 !!finalnames[[i]] := value)
      }
  }
# 2 Call -----------------------------------------------------------------
    if(i == 2) {

      #function to pre-create the MetricFilters list needed to iterate through the api alls
      load_dims <- function(dimItems) {
        mflist <- list(dimension = rep(dimensions[1:dimItems], each = metnumber))
        mflist <- append(mflist, values = list('type' = 'breakdown'))
        mflist <- append(mflist, values = list('id' = seq(length(mflist[[1]]))-1))
      }
      #run the function
      mfdims <- map(seq_along(dimensions)-1, load_dims)


      # a function that formats the list of metricFilters to run below the metricsContainer
      metricFiltersFunction <- function(i) {
        mfdimslist <-structure(list(id = mfdims[[i]]$id, type = 'breakdown', dimension = mfdims[[i]]$dimension, itemId = ''))
      }
      #map the function to list out the metricFiltres section of the api call
      lists_built <- map( seq_along(dimensions), metricFiltersFunction)

      ### function to create the breakdown 'metricsFilters'
      metricfilter_n <- function(filterId , type, dimension, itemId = '') {
        list(
          id = filterId,
          type = type,
          dimension = sprintf('variables/%s', dimension),
          itemId = itemId
        )
      }
      #run the list function to genereate the formated json string like list
      mflist <- list(lists_built[[i]]$id, lists_built[[i]]$type, lists_built[[i]]$dimension)


      mf_item <- pmap(mflist, metricfilter_n)

      mf_itemlist <- function(itemid) {
        map(mf_item, update_list, itemId = itemid)
      }

      api2 <- map(dat[[1]], mf_itemlist)

      req_bodies <- function(i, mf = api2) {
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
                       settings = list(
                         countRepeatInstances = TRUE,
                         limit = top[i],
                         page = 0,
                         nonesBehavior = unspecified
                       ),
                       statistics = list(
                         functions = c("col-max", "col-min")
                       )))
      }

      calls <- map2(i, api2, req_bodies)

      call_data_n <- function(calls) {
        if(debug == FALSE) {
          aa_call_data("reports/ranked", body = calls, company_id = company_id)
        } else if(debug == TRUE) {
          aa_call_data_debug("reports/ranked", body = calls, company_id = company_id)
        }
        if(debug == TRUE) {
          aa_call_data_debug("reports/ranked", body = calls, company_id = company_id)
        }
      }


      res <- map(calls, call_data_n)

      getdata <- function(it) {
        fromJSON(res[[it]])
      }

      res <- map(seq(length(res)),  getdata)

      t = 0

      el <- function(els) {
        if_else(res[[els]]$numberOfElements != 0, t+1, 0)
      }

      elnum <- sum(unlist(map(seq(length(res)), el)))

      rowsdata <- function(it, i) {
        res[[it]]$rows %>% mutate(!!prefinalnames[[1]][[1]] := dat[[1]][[it]],
                                  !!prefinalnames[[1]][[2]] := dat[[2]][[it]])
      }

      resrows <- map2_dfr(seq(elnum), i, rowsdata)

      #conditional statement to determine if the function should terminate and reurn the df or continue on.
      if(length(dimensions) != i) {
        ## second and not last data pull
        itemidname <- paste0('itemId_', dimensions[[i]])
        dat <- resrows %>%
          rename(!!itemidname := itemId,
                 !!finalnames[[i]] := value)
        dat <- dat %>% select(-data)

      } else {
        itemidname <- paste0('itemId_', dimensions[[i]])
        dat <- resrows %>%
          rename(!!itemidname := itemId,
                 !!finalnames[[i]] := value) %>%
          mutate(metrics = list(prefinalnames[[i+1]])) %>%
          unnest(c(metrics, data)) %>%
          spread(metrics, data) %>%
          select(finalnames)
        return(dat)
      }
    }

# N Calls -----------------------------------------------------------------
  if(i >= 3 && i <= length(dimensions)) {

      #function to pre-create the MetricFilters list needed to iterate through the api calls
      load_dims <- function(dimItems) {
        mflist <- list(dimension = rep(dimensions[1:dimItems], each = metnumber))
        mflist <- append(mflist, values = list('type' = 'breakdown'))
        mflist <- append(mflist, values = list('id' = seq(length(mflist[[1]]))-1))
      }
      #run the function
      mfdims <- map(seq_along(dimensions)-1, load_dims)


      # a function that formats the list of metricFilters too run below the metricsContainer
      metricFiltersFunction <- function(i) {
        mfdimslist <-structure(list(id = mfdims[[i]]$id, type = 'breakdown', dimension = mfdims[[i]]$dimension))
      }
      #map the function to list out the metricFiltres section of the api call
      lists_built <- map( seq_along(dimensions), metricFiltersFunction)

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
      mf_item <- pmap(mflist, metricfilter_n)

      #build the item ids needed for the next query
      mf_itemlist <- function(itemid) {
        ids <- map(map_depth(itemid, 1, unlist), rep,  each = length(metrics))
      }

      selectlist <- list()
      for(series in seq(i-1)){
        selectlist <-  append(selectlist, itemidnames[[series]])
      }
      itemidlist_n <- select(dat, unlist(selectlist))

      listum <- list()

      for(n_item in seq(nrow(itemidlist_n))) {
        listum <- append(listum, list(paste(itemidlist_n[n_item, ])))
      }
      itemidlist_n <- listum

      ##Create the itemids list in the correct number of times.
      itemidser <- map(itemidlist_n, mf_itemlist)

      ##join the 2 different itemids in their correct order. (ncapable)
      listing <- function(p = seq(itemidser)) {
        unlist(itemidser[[p]], use.names = F)
      }

      ##creating the list of lists for the appropriate number of metricFilter items (ncapable)
      itemidser <- map(seq(itemidser),  listing)

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
      calls <- map2(i, apicalls, req_bodies)

      #(ncapable)
      call_data_n <- function(calls) {
        if(debug == FALSE) {
          aa_call_data("reports/ranked", body = calls, company_id = company_id)
        }
        if(debug == TRUE) {
          aa_call_data_debug("reports/ranked", body = calls, company_id = company_id)
        }
      }

      #(ncapable)
      res <- map(calls, call_data_n)

      #(ncapable)
      getdata <- function(it) {
        fromJSON(res[[it]])
      }

      #(ncapable)
      resn <- map(seq(length(res)),  getdata)

    rowsdata <- function(it, i) {
        if(i == 2) {
          tf <- res[[it]]$rows %>% mutate(!!prefinalnames[[1]][[1]] := dat[[1]][[it]],
                                          !!prefinalnames[[1]][[2]] := dat[[2]][[it]])
          return(tf)
        }
        if(i == 3) {
          tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[2]][[1]] := dat[[1]][it],
                                           !!prefinalnames[[2]][[2]] := dat[[2]][it],
                                           !!prefinalnames[[1]][[1]] := dat[[3]][it],
                                           !!prefinalnames[[1]][[2]] := dat[[4]][it])
          return(tf)
        }
        if(i == 4) {
          tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[3]][[1]] := dat[[1]][it],
                                           !!prefinalnames[[3]][[2]] := dat[[2]][it],
                                           !!prefinalnames[[2]][[1]] := dat[[3]][it],
                                           !!prefinalnames[[2]][[2]] := dat[[4]][it],
                                           !!prefinalnames[[1]][[1]] := dat[[5]][it],
                                           !!prefinalnames[[1]][[2]] := dat[[6]][it])
          return(tf)
        }
        if(i == 5) {
          tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[4]][[1]] := dat[[1]][it],
                                           !!prefinalnames[[4]][[2]] := dat[[2]][it],
                                           !!prefinalnames[[3]][[1]] := dat[[3]][it],
                                           !!prefinalnames[[3]][[2]] := dat[[4]][it],
                                           !!prefinalnames[[2]][[1]] := dat[[5]][it],
                                           !!prefinalnames[[2]][[2]] := dat[[6]][it],
                                           !!prefinalnames[[1]][[1]] := dat[[7]][it],
                                           !!prefinalnames[[1]][[2]] := dat[[8]][it])
          return(tf)
        }
        if(i == 6) {
          tf <-  resn[[it]]$rows %>% mutate(!!prefinalnames[[5]][[1]] := dat[[1]][it],
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
        if(i == 7) {
          tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[6]][[1]] := dat[[1]][it],
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
        if(i == 8) {
          tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[7]][[1]] := dat[[1]][it],
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
        if(i == 9) {
          tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[8]][[1]] := dat[[1]][it],
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
        if(i == 10) {
          tf <- resn[[it]]$rows %>% mutate(!!prefinalnames[[9]][[1]] := dat[[1]][it],
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

      resrows <- map2_dfr(seq(length(resn)), i, rowsdata)


      if(length(dimensions) != i) {
        ## second and not last data pull
        itemidname <- paste0('itemId_', finalnames[[i]])
        dat <- resrows %>%
          rename(!!itemidname := itemId,
                 !!finalnames[[i]] := value)
        dat <- dat %>% select(-data)

      } else {
        itemidname <- paste0('itemId_', dimensions[[i]])
        dat <- resrows %>%
          rename(!!itemidname := itemId,
                 !!finalnames[[i]] := value) %>%
          mutate(metrics = list(prefinalnames[[i+1]])) %>%
          unnest(c(metrics, data)) %>%
          spread(metrics, data) %>%
          select(finalnames)
        return(dat)
      }
   }
  }
}


