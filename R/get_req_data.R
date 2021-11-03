#' Recursively query for data
#'
#' @param current_dim Current dimension being queried
#' @param item_ids Item IDs for previous dimensions
#' @param dimensions All dimensions to be queried
#' @param metrics Metrics in the request
#' @param rsid Reportsuite ID
#' @param global_filter Global filter list
#' @param settings Settings list
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
    company_id = company_id
  ))

  # Increment progress bar
  increment_global_counter()


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
#' # If no rows are returned, first argument will be empty list
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

