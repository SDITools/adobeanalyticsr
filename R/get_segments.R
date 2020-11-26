#' Get list of segments
#'
#' Retrieve all segments
#'
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' @param rsids Filter list to only include segments tied to specified RSID list (comma-delimited)
#' @param segmentFilter Filter list to only include suites in this RSID list (comma-delimited)
#' @param locale Locale (en_US default)
#' @param name Filter list to only include segments that contains the Name
#' @param tagNames Filter list to only include segments that contains one of the tags
#' @param filterByPublishedSegments Filter list to only include segments where the published field is set to one of the allowable values (all *default*, true, false)
#' @param limit Number of results per page (10 - *default*)
#' @param page Page number (base 0 - first page is "0")
#' @param sortDirection Sort direction (ASC *default* or DESC)
#' @param sortProperty Property to sort by (name, modified_date, id *default* is currently allowed)
#' @param expansion Comma-delimited list of additional segment metadata fields to include on response. (Options--reportSuiteName, ownerFullName, modified,tags, compatibility, definition, publishingStatus, definitionLastModified, categories)
#' @param includeType Include additional segments not owned by user. The "all" option takes precedence over "shared" (shared, all, templates)
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

