#' Get list of segments
#'
#' Retrieve all segments
#'
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' @param rsids Filter the list to only include segments tied to a specified RSID or
#' list of RSIDs. If including multiple RSIDs, they need to be a single, commas-separated string with no spaces.
#' @param segmentFilter Filter list to only include suites in this RSID list (comma-delimited)
#' @param locale The locale that segment details should be returned in. The default is `en_US`.
#' @param name Filter the list to only include segments that contain the specified **name**.
#' This is case-insensitive and is a simple, single string match.
#' @param tagNames Filter the list to only include segments that contain one of the tags.
#' @param filterByPublishedSegments Filter the list to only include segments where the published field is set to one of the allowable values:
#' `all` (the default), `TRUE`, or `FALSE`.
#' @param limit The number of results to return per page. This argument works in conjunction with the
#' `page` argument. The default is 10.
#' @param page The "page" of results to display. This works in conjunction with the `limit` argument and is
#' zero-based. For instance, if `limit = 20` and `page = 1`, the results returned would be 21 through 40.
#' @param sortDirection The sort direction for the results: `ASC` (default) for ascending or `DESC` for
#' descending. (This is case insensitive, so `asc` and `desc` work as well.)
#' #' @param sortProperty The property to sort the results by. Currently available values are `id` (default), `name`,
#' and `modified_date`. Note that setting `expansion = modified` returns results with a column added called
#' `modified`, which is the last date the calculated metric was modified. When using this value for `sortProperty`,
#' though, the name of the argument is `modified_date`, because why would we expect locked-in consistency
#' from Adobe?
#' @param expansion Comma-delimited list of additional segment metadata fields to include in
#' the results. Valid values include: `reportSuiteName`, `ownerFullName`, `modified`, `tags`, `compatibility`,
#' `definition`, `publishingStatus`, `definitionLastModified`, and `categories`. To include multiple values,
#' combine them into a single comma-separated string with _no spaces_.
#' @param includeType Include additional segments not owned by the user. Available values are `all` (default),
#' `shared`, and `templates`. The `all` option takes precedence over "shared"
#'
#' @return A data frame of segments and their meta data.
#'
#' @import stringr
#' @export
#'
aw_get_segments <- function(company_id = Sys.getenv("AW_COMPANY_ID"),
                          rsids = NA,
                          segmentFilter = NA,
                          locale = 'en_US',
                          name = NA,
                          tagNames = NA,
                          filterByPublishedSegments = 'all',
                          limit = 10,
                          page = 0,
                          sortDirection = 'ASC',
                          sortProperty = 'id',
                          expansion = NA,
                          includeType = 'all')
{
  #make the list of params into a dataframe
  if(length(rsids) > 1) {rsids = paste0(rsids, collapse = ',') }
  if(length(segmentFilter) > 1) {segmentFilter = paste0(segmentFilter, collapse = ',') }
  if(length(expansion) > 1) {expansion = paste0(expansion, collapse = ',') }

  vars <- tibble(rsids, segmentFilter, locale, name, tagNames, filterByPublishedSegments, limit, page, sortDirection,
  sortProperty, expansion, includeType)
  #Turn the list into a string to create the query
  prequery <- list(vars %>% select_if(~ !any(is.na(.))))
  #remove the extra parts of the string and replace it with the query parameter breaks
  query_param <- str_remove_all(str_replace_all(str_remove_all(paste(prequery, collapse = ''), '\\"'), ', ', '&'), 'list\\(| |\\)')

  #create the url to send with the query
  urlstructure <- paste0('segments?',query_param)

  #urlstructure <- 'segments?locale=en_US&filterByPublishedSegments=all&limit=1000&page=0&sortDirection=ASC&sortProperty=id&includeType=all'
  res <- aw_call_api(req_path = urlstructure[1], company_id = company_id)

  res <- jsonlite::fromJSON(res)

  #Just need the content of the returned json
  res <- res$content

  res

  }

