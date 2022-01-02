#' Get list of segments
#'
#' Retrieve all segments
#'
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use \code{\link{get_me}} to get a list of available `company_id` values.
#' @param rsids Filter the list to only include segments tied to a specified RSID or
#' list of RSIDs. Specify multiple RSIDs as a vector (i.e., "`rsids = c("rsid_1", rsid_2",...rsid_n")`").
#' Use \code{\link{aw_get_reportsuites}} to get a list of available `rsid` values.
#' @param segmentFilter Filter list to only include suites in this list of segment IDs (comma-delimited)
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
#' @param sortProperty The property to sort the results by. Currently available values are `id` (default), `name`,
#' and `modified_date`. Note that setting `expansion = modified` returns results with a column added called
#' `modified`, which is the last date the calculated metric was modified. When using this value for `sortProperty`,
#' though, the name of the argument is `modified_date`, because why would we expect locked-in consistency
#' from Adobe?
#' @param expansion Additional segment metadata fields to include in the results: `reportSuiteName`,
#' `ownerFullName`, `modified`, `tags`, `compatibility`, `definition`, `publishingStatus`, `definitionLastModified`,
#' and `categories`. This argument takes a single value (e.g., `expansion = "modified"`)
#' or a vector of values (e.g., `expansion = c("modified", "ownerFullName")`).
#' @param includeType Include additional segments not owned by the user. Available values are `all` (default),
#' `shared`, and `templates`. The `all` option takes precedence over "shared".
#' @param debug Include the output and input of the api call in the console for debugging. Default is FALSE
#'
#' @return A data frame of segments and their meta data.
#'
#' @export
#'
aw_get_segments <- function(company_id = Sys.getenv("AW_COMPANY_ID"),
                          rsids = NULL,
                          segmentFilter = NULL,
                          locale = 'en_US',
                          name = NULL,
                          tagNames = NULL,
                          filterByPublishedSegments = 'all',
                          limit = 10,
                          page = 0,
                          sortDirection = 'ASC',
                          sortProperty = 'id',
                          expansion = NULL,
                          includeType = 'all',
                          debug = FALSE)
{
  # Some NAs can be handled, but all NAs is regarded as an error
  assertthat::assert_that(is.null(rsids) | !all(is.na(rsids)))
  assertthat::assert_that(is.null(segmentFilter) | !all(is.na(segmentFilter)))
  assertthat::assert_that(is.null(name) | !all(is.na(name)))
  assertthat::assert_that(is.null(tagNames) | !all(is.na(tagNames)))
  assertthat::assert_that(is.null(expansion) | !all(is.na(expansion)))

  # paste(NULL, collapse = ",") produces a length 1 character vector, so have
  # to explicitly keep NULLs
  if (!is.null(rsids)) rsids <- paste(rsids, collapse = ",")
  if (!is.null(segmentFilter)) segmentFilter <- paste(segmentFilter, collapse = ",")
  if (!is.null(expansion)) expansion <- paste(expansion, collapse = ",")
  if (!is.null(tagNames)) tagNames <- paste0(tagNames, collapse = ',')
  if (!is.null(name)) name <- paste0(name, collapse = ',')

  query_params <- list(
    rsids = rsids,
    segmentFilter = segmentFilter,
    locale = locale,
    name = name,
    tagNames = tagNames,
    filterByPublishedSegments = filterByPublishedSegments,
    limit = limit,
    page = page,
    sortDirection = sortDirection,
    sortProperty = sortProperty,
    expansion = expansion,
    includeType = includeType
  )

  urlstructure <- paste('segments', format_parameters(query_params), sep = "?")
  res <- aw_call_api(req_path = urlstructure[1], debug = debug, company_id = company_id)

  jsonlite::fromJSON(res)$content
}



#' Format list as query parameters
#'
#' Pretty much ripped straight from `httr`, but it's not exported there so I
#' had to do this.
#'
#' @param elements Named list of query parameters
#'
#' @return String, `x` formatted as a parameter list
#' @noRd
format_parameters <- function(elements) {
  if (length(elements) == 0) {
    return("")
  }

  if (!is_fully_named_list(elements)) {
    stop("All components of query must be named", call. = FALSE)
  }

  stopifnot(is.list(elements))

  elements <- purrr::compact(elements)
  names <- utils::URLencode(names(elements))

  values <- vapply(
    elements,
    function(x) {
      if (inherits(x, "AsIs"))
        return(x)
      utils::URLencode(as.character(x))
    },
    character(1))

  paste0(names, "=", values, collapse = "&")
}



is_fully_named_list <- function(l) {
  if (length(names(l)[names(l) != ""]) != length(l)) {
    FALSE
  } else {
    TRUE
  }
}
