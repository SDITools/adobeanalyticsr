#' Create the Segment Predicate
#'
#' This function creates the simple predicate of a segment.
#'
#' @param subject This is the subject of the predicate. It is the dimension or metric id.
#' @param verb  Choose from any of the 30 different verbs
#' @param object This is the object of the predicate and answer the question what or how many
#' @param description The internal description for the predicate if wanted
#' @param is_distinct Count the distinct items to compare against instead of dimension number
#' @param attribution Define the type of attribution. Either `repeating` (default), `instance`, or `nonrepeating`
#' @param attribution_context When applying a non-repeating instance attribution model to a predicate the context for the attribution must be `visitors` (default) or `visits`
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used. Use [aw_get_reportsuites()] to get a list of available `rsid` values.
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#'
#' @return A structured list defining the predicate for a segment
#'
#' @import dplyr
#' @import assertthat
#' @import stringr
#' @importFrom glue glue
#'
#' @export
#'
seg_pred <- function(subject = 'page',
                     verb = 'contains',
                     object = 'pagename',
                     description = NULL,
                     is_distinct = FALSE,
                     attribution = 'repeating',
                     attribution_context = 'visitors',
                     rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                     company_id = Sys.getenv("AW_COMPANY_ID")){
  ### Define the different elements of a segment
  #########################
  ## Exists Verbs
  exists_verbs <- c('exists', 'not-exists', 'event-exists', 'not-event-exists')
  ## String Verbs
  str_verbs <- c('contains', 'not-contains', 'starts-with', 'ends-with', 'not-starts-with', 'not-ends-with', 'streq', 'not-streq', 'strlt', 'strgt', 'strle', 'strge')
  list_verbs <- c('streq-in', 'not-streq-in', 'contains-any-of', 'contains-all-of', 'not-contains-any-of', 'not-contains-all-of')
  glob_verbs <- c('matches', 'not-match') #uses regex in glob value
  ## Number Verbs
  num_verbs <- c('eq', 'not-eq', 'gt', 'lt', 'ge', 'le')
  numlist_verbs <- c('eq-any-of', 'not-eq-any-of', 'eq-in', 'not-eq-in')

  ##########################
  #assign the element to be either a variable or metric
  #pull the lists of metrics and dimensions if not available
  if(!exists('aw_metrics')){
    aw_metrics <- aw_get_metrics(company_id = company_id) }
  if(!exists('aw_dimensions')) {
    aw_dimensions <- aw_get_dimensions(company_id = company_id)
  }
  #define the variable to be either a metric or dimension and save it as the adjective
  adj <- dplyr::case_when(subject %in% aw_metrics$id ~ 'metrics/',
                          subject %in% aw_dimensions$id ~ 'variables/')
  assertthat::assert_that(!is.na(adj), msg = "The subject is not a valid element id.")
  ###/ element is valid
  ###If event/metric change exists to event exist verb
  if(adj == 'metrics/' && verb == 'exists'){
    verb <- 'event-exists'
  }
  if(adj == 'metrics/' && verb == 'not-exists'){
    verb <- 'not-event-exists'
  }
  #/event exists verb changed

  ##### Correct eq-any-of and not-eq-any-of from documentation
  if(verb == 'eq-any-of'){
    verb <- 'eq-in'
  }
  if(verb == 'not-eq-any-of'){
    verb <- 'not-eq-in'
  }
  #/fix equal any list for numbers

  # assert that the verb is on the list
  assertthat::assert_that(verb %in% c(exists_verbs, str_verbs, list_verbs, glob_verbs, num_verbs, numlist_verbs),
                          msg = "The 'verb' argument is not a valid verb. Use the function `seg_verb()` on https://www.adobe.io/apis/experiencecloud/analytics/docs.html#!AdobeDocs/analytics-2.0-apis/master/segments.md")

  ## define available date_subjects
  date_subjects <- c('daterangehour', 'daterangeday', 'daterangeweek', 'daterangemonth', 'daterangequarter', 'daterangeyear')

  #if subject is a date the object must be changed in the definition to the correct format
  if(subject %in% date_subjects){
    if(subject == 'daterangehour' && !verb %in% exists_verbs){
      #check if date and hour is included in object
      assertthat::assert_that(length(object) == 2 & grepl('-', object[[1]]), msg = "Make sure to object is a vector including the date and then time when using the 'daterangehour' subject. ex: c('2021-02-02', '1400')")
      #check the hour to make sure it is the correct format
      assertthat::assert_that(nchar(object[[2]]) == 4 & !is.na(as.numeric(object[[2]])), msg = "Make sure the hour is in military time. ex: '1500' if you wanted to use 3 PM as the hour" )
      minus100 <- as.character(as.numeric(stringr::str_remove_all(as.Date(object[[1]]), '-'))-100)
      hour <- str_extract(object[[2]], '\\d{2}') #use only the first two numbers for the hour
      object <- as.numeric(glue::glue("{stringr::str_replace(minus100, '20', '1')}{hour}"))
    }
    if(subject == 'daterangeday' && !verb %in% exists_verbs){
      assertthat::assert_that(class(as.Date(object, format="%Y-%m-%d")) == 'Date' & !is.na(as.Date(object, format="%Y-%m-%d")) & lubridate::year(as.Date(object)) %in% 1900:2500, msg = "Date must in YYYY-MM-DD format.")
      minus100 <- as.character(as.numeric(stringr::str_remove_all(as.Date(object), '-'))-100)
      object <- as.numeric(stringr::str_replace(minus100, '20', '1'))
    }
    if(subject == 'daterangemonth' && !verb %in% exists_verbs){
      #assert that the date is in the correct format
      assertthat::assert_that(class(as.Date(object, format="%Y-%m-%d")) == 'Date' & !is.na(as.Date(object, format="%Y-%m-%d")) & lubridate::year(as.Date(object)) %in% 1900:2500, msg = "Date must in YYYY-MM-DD format.")
      adjusted_date <- lubridate::floor_date(as.Date(object), unit = 'month' )
      minus100 <- as.character(as.numeric(stringr::str_remove_all((adjusted_date), '-'))-100)
      object <- as.numeric(stringr::str_replace(minus100, '20', '1'))
    }
    if(subject == 'daterangequarter' && !verb %in% exists_verbs){
      #assert that the date is in the correct format
      assertthat::assert_that(class(as.Date(object[[1]], format="%Y-%m-%d")) == 'Date' & !is.na(as.Date(object[[1]], format="%Y-%m-%d")) & lubridate::year(as.Date(object[[1]])) %in% 1900:2500, msg = "Date must in YYYY-MM-DD format.")
      ##assert that the fiscal start month is between 1-12
      if(length(object) == 2){
        assertthat::assert_that(object[[2]] %in% 1:12, msg = "Fiscal start month should be a number between 1-12. note: '1', January fiscal start, is default.")
      }
      #Assign the fiscalstart variable which adds the ability to adjust the fiscal floor date to match how it is setup in the Adobe Admin. The default is set to 1 which should cover a majority of situations.
      fiscalstart <- case_when(length(object) == 1 ~ '1', TRUE ~ object[2])
      adjusted_date <- lubridate::quarter(x = as.Date(object), type = "date_first", fiscal_start = as.numeric(fiscalstart))
      minus100 <- as.character(as.numeric(stringr::str_remove_all(adjusted_date, '-'))-100)
      object <- as.numeric(stringr::str_replace(minus100, '20', '1'))
    }
    if(subject == 'daterangeyear' && !verb %in% exists_verbs){
      #assert that the date is full date format
      assertthat::assert_that(grepl(object, '-'), msg = "The full date must be supplied. Any valid date within the desired year will work. ex: YYYY-MM-DD")
      #assert that the date is in the correct format
      assertthat::assert_that(class(as.Date(object, format="%Y-%m-%d")) == 'Date' & !is.na(as.Date(object, format="%Y-%m-%d")) & lubridate::year(as.Date(object)) %in% 1900:2500, msg = "Date must be supplied in the YYYY-MM-DD format.")
      adjusted_date <- lubridate::floor_date(as.Date(object), unit = 'year')
      minus100 <- as.character(as.numeric(stringr::str_remove_all((adjusted_date), '-'))-100)
      object <- as.numeric(stringr::str_replace(minus100, '20', '1'))
    }
  }
  #/end date formatting

  #Define the val func to be an attr or event
  if(adj == 'variables/'){
    val_func = 'attr'
  } else if(adj == 'metrics/'){
    val_func = 'event'
  }

  #This forms the predicate or pred
prepred <- if(verb %in%  exists_verbs && val_func == 'attr'){
    structure(list(
      func = verb,
      val = list(
        func = val_func,
        name = glue::glue('{adj}{subject}')
      )
    ))
  } else if(verb %in%  exists_verbs && val_func == 'event'){
    structure(list(
      func = verb,
      evt = list(
        func = val_func,
        name = glue::glue('{adj}{subject}')
      )
    ))
  } else if(verb %in% str_verbs){
    structure(list(
      func = verb,
      str = object,
      val = list(
        func = val_func,
        name = glue::glue('{adj}{subject}')
      )
    ))
  } else if(verb %in% list_verbs){
    structure(list(
      func = verb,
      list = object,
      val = list(
        func = val_func,
        name = glue::glue('{adj}{subject}')
      )
    ))
  } else if(verb %in% numlist_verbs){
    structure(list(
      func = verb,
      list = object,
      val = list(
        func = val_func,
        name = glue::glue('{adj}{subject}')
      )
    ))
  } else if(verb %in% glob_verbs){
    structure(list(
      func = verb,
      glob = object,
      val = list(
        func = val_func,
        name = glue::glue('{adj}{subject}')
      )
    ))
  } else if(verb %in% num_verbs && val_func == 'attr'){
    if(is_distinct) {
      structure(list(
        func = verb,
        num = object,
        val = list(
          func = 'total-distinct',
          field = list(
            func = val_func,
            name = glue::glue('{adj}{subject}')
          )
        )
      ))
    } else {
      structure(list(
        func = verb,
        num = object,
        val = list(
          func = val_func,
          name = glue::glue('{adj}{subject}')
        )
      ))
    }
  } else if(verb %in% num_verbs && val_func == 'event'){
    structure(list(
      func = verb,
      num = object,
      val = list(
        func = 'total',
        evt = list(
          func = val_func,
          name = glue::glue('{adj}{subject}')
        )
      )
    ))
  }
#add in the description
if(!is.null(description)){
  prepred$description = description
}
#add the context when necessary for proper attribution
if(attribution == 'instance') {
    prepred$val$`allocation-model`$func = 'allocation-instance'
} else if(attribution == 'nonrepeating'){
  if(attribution_context == 'hits') {
    warning('attribution_context cannot be set to `hits`, changing to default `visitors`')
    attribution_context <- 'visitors'
  }
  if(!is.null(attribution_context) && attribution_context == 'visits'){
    prepred$val$`allocation-model`$context = 'sessions'
    prepred$val$`allocation-model`$func = 'allocation-dedupedInstance'
  } else {
    prepred$val$`allocation-model`$context = attribution_context
    prepred$val$`allocation-model`$func = 'allocation-dedupedInstance'
  }
  }
return(prepred)
}
