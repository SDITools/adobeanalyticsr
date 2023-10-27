#' Create A Calculated Metric Formula
#'
#' Returns a JSON string formula to be used to build a calculated (derived) metric.
#'
#' @param operator Formula operators are divide (default), multiply, subtract, or add.
#' @param metrics The one or two metrics in a formula calculation
#' @param seg_filters A vector of segment filters to be added to a metric in the formula
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called
#' `AW_REPORTSUITE_ID` exists in `.Renviron` or elsewhere and no `rsid` argument
#' is provided, then the `AW_REPORTSUITE_ID` value will be used. Use [aw_get_reportsuites()]
#' to get a list of available `rsid` values.
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID`
#' exists in `.Renviron` or elsewhere and no `company_id` argument is provided,
#' then the `AW_COMPANY_ID` value will be used. Use [get_me()] to get a list of
#' available `company_id` values.
#'
#' @return A JSON string formula to be used in a calculated metric
#'
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom purrr map2
#' @export
#'
cm_formula <- function(operator = c('divide', 'multiply', 'subtract', 'add'),
                       metrics = c('visits', 'singlepagevisits'),
                       seg_filters = NA,
                       rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                       company_id = Sys.getenv("AW_COMPANY_ID")){

  if(length(operator) > 1) {
    op <- operator[[1]]
  } else {
    op <- operator
  }

  formula_items <- tidyr::as_tibble(c(metrics = list(metrics),
                                  seg_filters = dplyr::if_else(is.null(seg_filters), NA, list(seg_filters)))) |>
    dplyr::mutate(listtype = purrr::map(metrics, ~ attributes(.)$listtype),
                  cols = ifelse(length(metrics) > 1, c('col1', 'col2'), 'col1'))

  #assign the element to be either a variable or metric
  #pull the lists of metrics and dimensions if not available
  get_mets <- aw_get_metrics
  if (!memoise::is.memoised(get_mets)) {
    get_mets <- memoise::memoise(get_mets)
  }
  aw_metrics <- get_mets(company_id = company_id, rsid = rsid)
  #get the metric name
  formula_items$metricname <- purrr::map2_chr(formula_items$listtype, formula_items$metrics, \(x, y) {
    if(is.null(x)){
      aw_metrics$name[aw_metrics$id == y]
    } else {
      NA
    }
  })

  if(!is.null(seg_filters)) {
    filters <- purrr::map(formula_items$seg_filters, \(x)
                                 if(is.na(x) || x == '' ){
                                   NA
                                 } else {
                                   list(func = "calc-metric",
                                        version = c(1, 0, 0),
                                        filters = list(
                                          list(
                                            func = 'segment-ref',
                                            description = 'seq test',
                                            id = x
                                          )
                                        )
                                   )
                                 }
    )
    #get the pmap list values
    col_lists <- list(filters, seq(formula_items$metrics), formula_items$metrics, formula_items$metricname, formula_items$listtype)

    cols <- purrr::pmap(col_lists, function(filter, metric_number, metric_item, met_name, listtype)
                          if(!is.list(filter) & is.null(listtype)) {
                            list(func = 'metric',
                                 name = glue::glue('metrics/{metric_item}'),
                                 description = met_name)
                          } else if (!is.list(filter) & !is.null(listtype)){
                            formula_items$metrics[[metric_number]]
                          } else {
                            c(filter,
                              list(formula = list(
                                func = 'metric',
                                name = glue::glue('metrics/{metric_item}'),
                                description = met_name)))
                          }
                        )

  } else if(is.na(seg_filters) & is.null(formula_items$listtype)) {

    cols <- purrr::map2(formula_items$metrics,
                        formula_items$metricname,
                        \(x, y) list(func = 'metric',
                                     name = glue::glue('metrics/{x}'),
                                     description = y))
  }

  #if there is more than 1 metric then there will be > 1 cols
  if(length(cols) > 1) {
    fm <- list(func = op,
         col1 = cols[[1]],
         col2 = cols[[2]])
  } else {
    fm <- cols[[1]]
  }
  structure(fm, listtype = 'cm_formula' )
}
