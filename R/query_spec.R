#' Make a query specification
#'
#' @description
#' A query spec contains the information about the query. This comes with getters
#' that will always return the correct element, even if the underlying data
#' structure changes.
#'
#' Getters for indexable items (top, page, search dimensions) allow you to return
#' either all items or a range of items. This is simple indexing -- you can use
#' a range or get a single item, or no items.
#'
#' @noRd
make_query_spec <- function(
  rsid,
  company_id,
  dimensions,
  metrics,
  global_filter,
  limit,
  page,
  search,
  sort,
  nonesBehavior,
  dimensionSort
) {
  list(
    rsid = rsid,
    company_id = company_id,
    global_filter = global_filter,
    settings = list(
      top = limit,
      page = page,
      nonesBehavior = nonesBehavior,
      dimensionSort = "asc"
    ),
    search = search,
    dimensions = dimensions,
    metrics = metrics,
    sort = sort
  )
}


qs_rsid <- function(qs) {
  qs$rsid
}

qs_company_id <- function(qs) {
  qs$company_id
}

qs_global_filter <- function(qs) {
  qs$global_filter
}

qs_top <- function(qs, i = NULL) {
  if (is.null(i)) {
    return(qs$settings$top)
  } else {
    return(qs$settings$top[i])
  }
}

qs_page <- function(qs, i = NULL) {
  if (is.null(i)) {
    return(qs$settings$page)
  } else {
    return(qs$settings$page[i])
  }
}

qs_search <- function(qs, i = NULL) {
  if (is.null(i)) {
    return(qs$search)
  } else {
    return(qs$search[i])
  }
}

qs_dimensions <- function(qs, i = NULL) {
  if (is.null(i)) {
    return(qs$dimensions)
  } else {
    return(qs$dimensions[i])
  }
}

qs_metrics <- function(qs) {
  qs$metrics
}

qs_nones_behavior <- function(qs) {
  qs$settings$nonesBehavior
}

qs_dimension_sort <- function(qs) {
  qs$settings$dimensionSort
}


qs_sort <- function(qs) {
  qs$sort
}