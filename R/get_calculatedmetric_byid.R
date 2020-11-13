#' Get a single calculated metric.
#'
#' Retrieve one calculated metric
#'
#' @param company_id Company Id.  Taken from the global environment by default if not provided.
#' @param id The calculated metric ID to retrieve - required
#' @param locale Locale. Default = en_US
#' @param expansion Comma-delimited list of additional calculated metric metadata fields to include on response. (reportSuiteName, ownerFullName, modified, tags, definition, categories)
#'
#' @import stringr
#' @export
#'
aa_get_calculatedmetric_byid <- function(company_id = Sys.getenv("AA_COMPANY_ID"),
                          id = 'cm300006896_5fac6262d1a4a8555835dc5c',
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
  res <- aa_call_api(req_path = urlstructure[1], company_id = company_id)

  res <- fromJSON(res)

  #Just need the content of the returned json
  res <- res$content

  res

  }

