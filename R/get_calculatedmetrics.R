#' Get list of calculated metrics
#'
#' Retrieve All Calculated Metrics
#'
#' @param company_id Company Id.  Taken from the global environment by default if not provided.
#' @param rsids Filter list to only include calculated metrics tied to specified RSID list (comma-delimited)
#' @param ownerId Filter list to only include calculated metrics owned by the specified loginId
#' @param filterByIds Filter list to only include calculated metrics in the specified list (comma-delimited list of IDs) (this is the same as calculatedMetricFilter, and is overwritten by calculatedMetricFilter
#' @param toBeUsedInRsid The report suite where the calculated metric intended to be used. This report suite will be used to determine things like compatibility and permissions. If it is not specified then the permissions will be calculated based on the union of all metrics authorized in all groups the user belongs to. If the compatibility expansion is specified and toBeUsedInRsid is not then the compatibility returned is based off the compatibility from the last time the calculated metric was saved.
#' @param locale Locale (en_US default)
#' @param name Filter list to only include calculated metrics that contains the Name
#' @param tagNames Filter list to only include calculated metrics that contains one of the tags
#' @param favorite Filter list to only include calculated metrics that are favorites (boolean)
#' @param approved Filter list to only include calculated metrics that are approved (boolean)
#' @param limit Number of results per page
#' @param page Page number (base 0 - first page is "0")
#' @param sortDirection Sort direction (ASC or DESC)
#' @param sortProperty Property to sort by (name, modified_date, id is currently allowed)
#' @param expansion Comma-delimited list of additional calculated metric metadata fields to include on response. (options = reportSuiteName, ownerFullName, modified, tags, definition, compatability, categories)
#' @param includeType Include additional calculated metrics not owned by user. The "all" option takes precedence over "shared" (options = all, shared, templates)
#' @param debug
#'
#' @import stringr
#' @export
#'
aw_get_calculatedmetrics <- function(company_id = Sys.getenv("AW_COMPANY_ID"),
                          rsids = NA,
                          ownerId = NA,
                          filterByIds = NA,
                          toBeUsedInRsid = NA,
                          locale = "en_US",
                          name = NA,
                          tagNames = NA,
                          favorite = NA,
                          approved = NA,
                          limit = 1000,
                          page = 0,
                          sortDirection = 'DESC',
                          sortProperty = NA,
                          expansion = NA,
                          includeType = 'all',
                          debug = FALSE)
{
  #edit the character vectors to the string they need to be
  if(length(rsids) > 1) {rsids = URLencode(paste0(rsids, collapse = ',')) }
  if(length(filterByIds) > 1) {filterByIds = paste0(filterByIds, collapse = ',') }
  if(!is.na(tagnames)) {tagnames = URLencode(paste0(tagnames, collapse = ',')) }
  if(length(expansion) > 1) {expansion = paste(expansion, collapse = ',', sep = '') }

  #includeType is case senstative
  includeType <- tolower(includeType)

  vars <- tibble::tibble(rsids, ownerId, filterByIds, toBeUsedInRsid, locale, name, tagnames, favorite, approved,
                         limit, page, sortDirection, sortProperty, expansion, includeType)
  #Turn the list into a string to create the query
  prequery <- vars %>% select_if(~ !any(is.na(.)))
  #remove the extra parts of the string and replace it with the query parameter breaks
  query_param <-  paste(names(prequery), prequery, sep = '=', collapse = '&')

  #create the url to send with the query
  urlstructure <- paste0('calculatedmetrics?',query_param)

  #urlstructure <- 'segments?locale=en_US&filterByPublishedSegments=all&limit=1000&page=0&sortDirection=ASC&sortProperty=id&includeType=all'
  res <- aw_call_api(req_path = urlstructure[1], debug = debug, company_id = company_id)

  res <- jsonlite::fromJSON(res)

  #Just need the content of the returned json
  res <- res$content

  res

  }

