#' Create the Segment Predicate
#'
#' This function creates the simple predicate of a segment.
#'
#' @param subject This is the subject of the predicate. It is the dimension or metric id.
#' @param verb Choose from any of the 30 different verbs. Use the `seg_verbs()` function to see all available verbs along with the descriptions.
#' @param object This is the object of the predicate and answers the question `what` or `how many`
#' @param description The internal description for the predicate. (optional) This will not show in the UI but could be very helpful when using the API.
#' @param is_distinct This will segment on a distinct count of items within a dimension. Examples: “Visitors who viewed more than 5 distinct products,” or “Visits where more than 5 distinct pages were seen.”
#' @param attribution Define the type of attribution. Either `repeating` (default), `instance`, or `nonrepeating`. See Details for more information.
#' @param attribution_context When applying a non-repeating instance attribution model to a predicate the context for the attribution must be `visitors` (default) or `visits`
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used. Use [aw_get_reportsuites()] to get a list of available `rsid` values.
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#'
#' @details
#' **Attribution Models**
#' Available for dimensions only, these models determine what values in a dimension to segment for.
#' Dimension models are particularly useful in sequential segmentation.
#' - *repeating* (default): Includes instances and persisted values for the dimension.
#' - *instance*: Includes instances for the dimension.
#' - *nonrepeating* instance: Includes unique instances (non-repeating) for the dimension. This is the model applied in Flow when repeat instances are excluded.
#'
#' @return A structured list defining the predicate for a segment
#'
#' @import dplyr
#' @import assertthat
#' @import stringr
#' @importFrom glue glue
#' @importFrom memoise memoise
#' @export
seg_pred <- function(subject = 'page',
                     verb = 'contains',
                     object = 'home',
                     description = NULL,
                     is_distinct = FALSE,
                     attribution = 'repeating',
                     attribution_context = 'visitors',
                     rsid = Sys.getenv("AW_REPORTSUITE_ID"),
                     company_id = Sys.getenv("AW_COMPANY_ID")){
  ### Define the different elements of a segment
  #########################
  verbs <- seg_verbs()[, 1:3]
  ##########################
  #assign the element to be either a variable or metric
  #pull the lists of metrics and dimensions if not available
  get_mets <- aw_get_metrics
  if (!memoise::is.memoised(get_mets)) {
    get_mets <- memoise::memoise(get_mets)
  }
  aw_metrics <- get_mets(company_id = company_id, rsid = rsid)
  get_dims <- aw_get_dimensions
  if (!memoise::is.memoised(get_dims)) {
    get_dims <- memoise::memoise(get_dims)
  }
  aw_dimensions <- get_dims(company_id = company_id, rsid = rsid)

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
  #fix equal any list for numbers

  # assert that the verb is on the list
  assertthat::assert_that(verb %in% verbs$verb,
                          msg = "The 'verb' argument is not a valid verb. Use the function `seg_verb()` to see all available verbs or visit Adobe Experience League for more information. https://www.adobe.io/apis/experiencecloud/analytics/docs.html#!AdobeDocs/analytics-2.0-apis/master/segments.md")
  # Change Date Range to correct format
  hour_change <- function(object) {
    minus100 <- as.character(as.numeric(stringr::str_remove_all(as.Date(object[[1]]), '-'))-100)
    hour <- stringr::str_extract(object[[2]], '\\d{2}') #use only the first two numbers for the hour
    object <- as.numeric(glue::glue("{stringr::str_replace(minus100, '20', '1')}{hour}"))
    object
  }
  day_change <- function(object) {
    minus100 <- as.character(as.numeric(stringr::str_remove_all(as.Date(object), '-'))-100)
    object <- as.numeric(stringr::str_replace(minus100, '20', '1'))
    object
  }
  month_change <- function(object) {
    adjusted_date <- lubridate::floor_date(as.Date(object), unit = 'month' )
    minus100 <- as.character(as.numeric(stringr::str_remove_all((adjusted_date), '-'))-100)
    object <- as.numeric(stringr::str_replace(minus100, '20', '1'))
    object
  }
  quarter_change <- function(object) {
    #Assign the fiscalstart variable which adds the ability to adjust the fiscal floor date to match how it is setup in the Adobe Admin. The default is set to 1 which should cover a majority of situations.
    fiscalstart <- case_when(length(object) == 1 ~ '1', TRUE ~ object[2])
    adjusted_date <- lubridate::quarter(x = as.Date(object), type = "date_first", fiscal_start = as.numeric(fiscalstart))
    minus100 <- as.character(as.numeric(stringr::str_remove_all(adjusted_date, '-'))-100)
    object <- as.numeric(stringr::str_replace(minus100, '20', '1'))
    object
  }
  year_change <- function(object) {
    adjusted_date <- lubridate::floor_date(as.Date(object), unit = 'year')
    minus100 <- as.character(as.numeric(stringr::str_remove_all((adjusted_date), '-'))-100)
    object <- as.numeric(stringr::str_replace(minus100, '20', '1'))
    object
  }
  # Daterange subjects must have correct format to be referenced properly in the
  # API calls
  daterange_change <- function(subject, object, verbs){
    if(subject == 'daterangehour' && !verb %in% verbs$verb[verbs$class == 'exists']){
      #check if date and hour is included in object
      assertthat::assert_that(length(object) == 2 & grepl('-', object[[1]]), msg = "Make sure to object is a character vector including the date and then time when using the 'daterangehour' subject. ex: c('2021-02-02', '1400')")
      #check the hour to make sure it is the correct format
      assertthat::assert_that(nchar(object[[2]]) == 4 & !is.na(as.numeric(object[[2]])), msg = "Make sure the hour is in military time. ex: '1500' if you wanted to use 3 PM as the hour" )

      object <- hour_change(object)
    }
    if(subject == 'daterangeday' && !verb %in% verbs$verb[verbs$class == 'exists']){
      assertthat::assert_that(class(as.Date(object, format="%Y-%m-%d")) == 'Date' & !is.na(as.Date(object, format="%Y-%m-%d")) & lubridate::year(as.Date(object)) %in% 1900:2500, msg = "Date must in YYYY-MM-DD format.")

      object <- day_change(object)
    }
    if(subject == 'daterangemonth' && !verb %in% verbs$verb[verbs$class == 'exists']){
      #assert that the date is in the correct format
      assertthat::assert_that(class(as.Date(object, format="%Y-%m-%d")) == 'Date' & !is.na(as.Date(object, format="%Y-%m-%d")) & lubridate::year(as.Date(object)) %in% 1900:2500, msg = "Date must in YYYY-MM-DD format.")

      object <- month_change(object)
    }
    if(subject == 'daterangequarter' && !verb %in% verbs$verb[verbs$class == 'exists']){
      #assert that the date is in the correct format
      assertthat::assert_that(class(as.Date(object[[1]], format="%Y-%m-%d")) == 'Date' & !is.na(as.Date(object[[1]], format="%Y-%m-%d")) & lubridate::year(as.Date(object[[1]])) %in% 1900:2500, msg = "Date must in YYYY-MM-DD format.")
      ##assert that the fiscal start month is between 1-12
      if(length(object) == 2){
        assertthat::assert_that(object[[2]] %in% 1:12, msg = "Fiscal start month should be a number between 1-12. note: '1', January fiscal start, is default.")
      }

      object <- quarter_change(object)
    }
    if(subject == 'daterangeyear' && !verb %in% verbs$verb[verbs$class == 'exists']){
      #assert that the date is full date format
      assertthat::assert_that(grepl(object, '-'), msg = "The full date must be supplied. Any valid date within the desired year will work. ex: YYYY-MM-DD")
      #assert that the date is in the correct format
      assertthat::assert_that(class(as.Date(object, format="%Y-%m-%d")) == 'Date' & !is.na(as.Date(object, format="%Y-%m-%d")) & lubridate::year(as.Date(object)) %in% 1900:2500, msg = "Date must be supplied in the YYYY-MM-DD format.")

      object <- year_change(object)
    }
  }
  #/end date formatting
  #if subject is one of 'daterange' the object must be changed in the definition to the correct format
  if (subject %in% c('daterangehour', 'daterangeday', 'daterangeweek', 'daterangemonth', 'daterangequarter', 'daterangeyear')){
    object <- daterange_change(subject, object, verbs)
  }

  #Define the val func to be an attr or event
  define_val_func <- function(adj){
    if (adj == 'variables/'){
      'attr'
    } else {
      'event'
    }
  }

  val_func <- define_val_func(adj)

create_pred <- function(val_func, verbs, is_distinct) {
  #define reference for verbs
  exists_verbs <- verbs$verb[verbs$type == 'exists' & verbs$class == 'exists']
  str_verbs <- verbs$verb[verbs$type == 'string' & verbs$class == 'string']
  strlist_verbs <- verbs$verb[verbs$type == 'string' & verbs$class == 'list']
  numlist_verbs <- verbs$verb[verbs$type == 'number' & verbs$class == 'list']
  num_verbs <- verbs$verb[verbs$type == 'number' & verbs$class == 'number']
  glob_verbs <- verbs$verb[verbs$type == 'string' & verbs$class == 'glob']

  if (verb %in%  exists_verbs && val_func == 'attr'){
    structure(list(
      func = verb,
      val = list(
        func = val_func,
        name = glue::glue('{adj}{subject}')
      )
    ))
  } else if (verb %in%  exists_verbs && val_func == 'event'){
    structure(list(
      func = verb,
      evt = list(
        func = val_func,
        name = glue::glue('{adj}{subject}')
      )
    ))
  } else if (verb %in% str_verbs){
    structure(list(
      func = verb,
      str = object,
      val = list(
        func = val_func,
        name = glue::glue('{adj}{subject}')
      )
    ))
  } else if (verb %in% strlist_verbs){
    structure(list(
      func = verb,
      list = object,
      val = list(
        func = val_func,
        name = glue::glue('{adj}{subject}')
      )
    ))
  } else if (verb %in% numlist_verbs){
    structure(list(
      func = verb,
      list = object,
      val = list(
        func = val_func,
        name = glue::glue('{adj}{subject}')
      )
    ))
  } else if (verb %in% glob_verbs){
    structure(list(
      func = verb,
      glob = object,
      val = list(
        func = val_func,
        name = glue::glue('{adj}{subject}')
      )
    ))
  } else if (verb %in% num_verbs && val_func == 'attr'){
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
}

#This forms the predicate or pred
prepred <- create_pred(val_func, verbs, is_distinct)

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
prepred
}
