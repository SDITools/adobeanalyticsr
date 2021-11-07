#' Query quickly for segments
#'
#' Quickly query for many segments and the same metrics. This is the
#' equivalent of a freeform table with segments as the row components.
#'
#' @inheritParams aw_freeform_table
#' @param globalSegment One or more segments to apply globally
#' @param segmentIds One or more segments that will compose the rows of the
#'   table.
#'
#' @export
aw_segment_table <- function(company_id = Sys.getenv("AW_COMPANY_ID"),
                             rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                             date_range = c(Sys.Date()-30, Sys.Date()-1),
                             metrics = c("visits", "visitors"),
                             globalSegment = NULL,
                             segmentIds = NULL,
                             metricSort =  'desc',
                             prettynames = FALSE,
                             debug = FALSE) {
  if (length(segmentIds) == 0) {
    stop("At least one segment ID must be given", call. = FALSE)
  }
  # Generate requests
  # 1 request group for each unique metric
  # Page the segments into groups of 9 or 10

  # Initialize global counter
  n_requests <- length(cut_segments(segmentIds)) * length(metrics)
  if (n_requests > 1) {
    initialize_global_counter(n_requests)
  } else {
    kill_global_counter()
  }

  message(glue::glue("Getting {length(metrics)} metrics for {length(segmentIds)} segments..."))
  metric_result_list <- purrr::map(metrics, function(met) {
    seg_id_groups <- cut_segments(segmentIds)

    purrr::map_dfr(seg_id_groups, function(seg_group) {
      out <- aw_segment_table_page(company_id = company_id,
                            rsid = rsid,
                            date_range = date_range,
                            metrics = met,
                            globalSegment = globalSegment,
                            segmentIds = seg_group,
                            metricSort =  metricSort,
                            prettynames = prettyNames,
                            debug = debug)

      increment_global_counter()
      out
    })
  })

  message("Done!")

  join_metric_cols <- purrr::partial(left_join, by = "name")
  Reduce(join_metric_cols, metric_result_list)
}


#' Generate breaks for cut
#'
#' @param len Length of vector, to be cut into groups of 10
#'
#' @return Vector of breaks to pass to `cut`
#' @noRd
#' @example
#' cut_breaks(20)
#' cut(seq(20), breaks = cut_breaks(20))
cut_breaks <- function(len) {
  c(seq(0.5, len, by = 10), len + 0.5)
}


#' Generate a list of data frames of segments
#'
#' This is the paging function, which takes a list of segments and splits them
#' into groups of at most 10.
#'
#' @param segs Vector of segments
#'
#' @return List of vectors of segment IDs, in groups of no more than 10
#' @noRd
#' @examples
#' segs <- c("12345", "67890")
#' more_segs <- rep(segs, 18)
#'
#' make_cuts(segs)
#' make_cuts(more_segs)
cut_segments <- function(segs) {
  seg_seq <- seq_along(segs)

  split(
    x = segs,
    f = cut(seg_seq, breaks = cut_breaks(length(segs)), labels = FALSE)
  )
}


#' Make pretty segment names for the segments
#'
#' @param rsid Report Suite ID
#' @param company_id Company ID
#' @param df Data frame with with at least the column `segmentIds`
#'
#' @return `df`, with pretty name information left-joined onto it
#' @noRd
make_pretty_segments <- function(rsid,
                                 company_id,
                                 df) {
  # Lookup segment names
  segs <- aw_get_segments(company_id = company_id,
                  rsids = rsid,
                  segmentFilter = df$segmentIds) %>%
    select(id, name)

  left_join(df, segs, by = c("segmentIds" = "id")) %>%
    select(name, value, metrics)
}


#' Query for a segment table
#'
#' Responsible for actually querying (one page of) segment rows, this is called
#' once for each metric and group of segments.
#'
#' @inheritParams aw_segment_table
#' @noRd
aw_segment_table_page <- function(company_id = Sys.getenv("AW_COMPANY_ID"),
                             rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                             date_range = c(Sys.Date()-30, Sys.Date()-1),
                             metrics = c("visits", "visitors"),
                             globalSegment = NULL,
                             segmentIds = NULL,
                             metricSort =  'desc',
                             prettynames = FALSE,
                             debug = FALSE,
                             check_components = TRUE) {
  metrics <- unique(metrics)

  # Make global filter
  timeframe <- make_timeframe(date_range)

  gf <- global_filter(
    type = c("dateRange", rep("segment", times = length(globalSegment))),
    dateRange = c(timeframe, rep(NA, times = length(globalSegment))),
    segmentId = c(NA, globalSegment)
  )

  # Define settings
  settings <- list(dimensionSort = "asc")

  # For each metric/segment combination, create a metric entry in the
  # metric container
  seg_ctrl <- tidyr::expand_grid(
    metrics = metrics,
    segmentIds = segmentIds
  ) %>%
    group_by(metrics) %>%
    mutate(
      metric_id = paste(metrics, row_number(), sep = "::")
    ) %>%
    ungroup()

  # Generate metric container
  met_cont <- metric_container(
    metrics = seg_ctrl$metrics,
    metricIds = seg_ctrl$metric_id,
    sort = "desc",
    segmentIds = seg_ctrl$segmentIds
  )

  req <- list(
    rsid = rsid,
    globalFilters = gf,
    metricContainer = met_cont,
    settings = settings
  )

  # jsonlite::toJSON(req, pretty = TRUE, auto_unbox = TRUE)

  output_data <- jsonlite::fromJSON(aw_call_data(
    req_path = "reports/ranked",
    body = req,
    debug = debug,
    company_id = company_id
  ))

  long_metrics <- data.frame(
    name = output_data$columns$columnIds,
    value = output_data$summaryData$totals
  )

  output_data <- left_join(seg_ctrl, long_metrics, by = c("metric_id" = "name"))
  output_data <- make_pretty_segments(rsid = rsid,
                       company_id = company_id,
                       df = output_data)

  output_data %>%
    pivot_wider(names_from = metrics, values_from = value)
}
