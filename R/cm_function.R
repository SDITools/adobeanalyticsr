#' Create A Calculated Metric Function
#'
#' Returns a JSON string defining a function to be used to build a calculated (derived) metric.
#'
#' @param func Calculated function id. Only Basic single metric functions are able to be used.
#' @param metric The metric to be used in the functional metric calculation. Default is `visits`
#' @param dimension The dimension to be used in the functional metric calculation. Default is `NULL`
#' @param seg_filter A segment filter to be added to a metric in the formula
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called
#' `AW_REPORTSUITE_ID` exists in `.Renviron` or elsewhere and no `rsid` argument
#' is provided, then the `AW_REPORTSUITE_ID` value will be used. Use [aw_get_reportsuites()]
#' to get a list of available `rsid` values.
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID`
#' exists in `.Renviron` or elsewhere and no `company_id` argument is provided,
#' then the `AW_COMPANY_ID` value will be used. Use [get_me()] to get a list of
#' available `company_id` values.
#'
#' @return Returns a full list of calculated metric functions or a speicified function that the user can access.
#'
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom purrr map2
#' @export
#'
cm_function <- function(func = 'col-sum',
                        metric = 'visits',
                        dimension = NULL,
                        seg_filter = NULL,
                        rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                        company_id = Sys.getenv("AW_COMPANY_ID")){

  # func = 'col-sum'
  # # func = NULL
  # # metric = NULL
  # # seg_filter = NULL
  # metric <- 'visits' ##checking
  # seg_filter = 's300003965_621be928fb65157b4202b6c6'
  # rsid = Sys.getenv("AW_REPORTSUITE_ID")
  # company_id = Sys.getenv("AW_COMPANY_ID")

  #assert that the 2 key arguments have values
  assertthat::assert_that(assertthat::not_empty(func), msg = 'Argument "func" cannot be empty')

  #collect the calculated metric function information
  functioninfo <- get_cm_functions(func, company_id = company_id)

  #assign the element to be either a variable or metric
  #pull the lists of metrics and dimensions if not available
  get_mets <- aw_get_metrics
  if (!memoise::is.memoised(get_mets)) {
    get_mets <- memoise::memoise(get_mets)
  }
  aw_metrics <- get_mets(company_id = company_id, rsid = rsid)

  #create the metric object
  if(func == 'count-distinct-metric') {
    metricobject <- list(func = 'attr',
                         name = glue::glue('variables/{dimension}'))
  } else {
    metricobject <- list(func = 'metric',
                         name = glue::glue('metrics/{metric}'),
                         description = aw_metrics$name[aw_metrics$id == metric])
  }
  #create the segment object
  if(!is.null(seg_filter)) {
    segmentobject <- list(func = 'segment-ref',
                          description = aw_get_segments(segmentFilter = seg_filter,
                                                        company_id = company_id,
                                                        expansion = 'name')$name,
                          id = seg_filter)
  }

  #create the final object if needed
  if(!is.null(seg_filter)) {
    filtered_object <-
      list(
        func = 'calc-metric',
        formula = metricobject,
        version = list(1, 0, 0),
        filters = list(segmentobject)
    )
  }
  if(func == 'count-distinct-metric') {
    function_object <- c(
      func = func,
      description = get_cm_functions(id = func)$name,
      field = ifelse(!is.null(seg_filter), list(filtered_object), list(metricobject))
    )
  } else {
    function_object <- c(
      func = func,
      description = get_cm_functions(id = func)$name,
      col = ifelse(!is.null(seg_filter), list(filtered_object), list(metricobject))
    )
  }

  structure(function_object, listtype = 'cm_function' )
}
