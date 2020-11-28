#' Get a single calculated metric
#'
#' Retrieve a single calculated metric and meta data about it.
#'
#' @details
#' This function is useful for getting details about a specific calculated metric based on the metric's ID.
#'
#' The `expansion` argument accepts the following values, which will then include additional columns
#' in the results:
#'
#' * **ownerFullName: adds `owner.name` and `owner.login` columns to the results (`owner.id` is
#' already included by default).
#'
#' * **modified**: adds a `modified` column to the output with the date (ISO 8601 format) each
#' calculated metric was last modified.
#'
#' * **definition**: adds _multiple_ columns (the number will vary based on the number and complexity
#' of calculated metrics returns) that provide the actual formula for each of the calculated metrics.
#' This is returned from the API as a JSON object and converted into columns by the function, which
#' means it is pretty messy, so, really, it's not recommended that you use this value.
#'
#' * **compatability**: should add a column with the products that the metric is compatibile with, but this
#' behavior has not actually been shown to be true, so this may actually do nothing if included.
#'
#' * **reportSuiteName**: adds a `reportSuiteName` and a `siteTitle` column with the friendy report
#' suite name for the RSID.
#'
#' * **tags**: adds a column with an embedded data frame with all of the existing tags that are
#' associated with the calculated metric. This can be a bit messy to work with, but the information
#' is, at least, there.
#'
#' Multiple values for `expansion` can be included in the argument as a comma-separated value. For instance,
#' `expansion = "tags,modified"` will add both a `tags` column and a `modified` column to the output.
#'
#' @seealso \code{\link{aw_get_calculatedmetrics}}, \code{\link{aw_get_metrics}}
#'
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' @param id The calculated metric ID to retrieve. This is a required field.
#' @param locale The locale that system-named metrics should be returned in. Non-localized values will
#' be returned for title, name, description, etc. if a localized value is not available.
#' @param expansion Comma-delimited list of additional calculated metric metadata fields to include in
#' the results. Valid values include: **reportSuiteName**, **ownerFullName**, **modified**, **tags**,
#' **definition**, **compatability**, **categories**. See **Details** for more information about the
#' quirks and oddities of this argument.
#'
#' @import stringr
#' @export
#'
aw_get_calculatedmetric_byid <- function(company_id = Sys.getenv("AW_COMPANY_ID"),
                          id = NA,
                          locale = "en_US",
                          expansion = NA
                          )
{
  #make the list of params into a dataframe
  if(length(expansion) > 1) {expansion = paste0(expansion, collapse = ',') }

  vars <- tibble(locale, expansion)
  #Turn the list into a string to create the query
  prequery <- list(vars %>% select_if(~ !any(is.na(.))))
  #remove the extra parts of the string and replace it with the query parameter breaks
  query_param <- str_remove_all(str_replace_all(str_remove_all(paste(prequery, collapse = ''), '\\"'), ', ', '&'), 'list\\(| |\\)')

  #create the url to send with the query
  urlstructure <- paste0('calculatedmetrics/', id,'?',query_param)

  #urlstructure <- 'segments?locale=en_US&filterByPublishedSegments=all&limit=1000&page=0&sortDirection=ASC&sortProperty=id&includeType=all'
  res <- aw_call_api(req_path = urlstructure[1], company_id = company_id)

  res <- fromJSON(res)

  #Just need the content of the returned json
  res <- res$content

  res

  }

