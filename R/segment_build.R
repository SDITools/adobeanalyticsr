#' Build the Segment
#'
#' This function combines predicates into a container.
#'
#' @param name This is the name of the new segment
#' @param description  This is the description of the segment
#' @param containers List of the container(s) that make up the segment
#' @param predicates List of the predicate(s) to create a segment
#' @param sequences List of the predicates and sequence containers that are combined to make a segment
#' @param context This defines the main container context, either hits, visits (Default), or visitors
#' @param conjunction This will tell how the different containers and predicates should be compared. Use either 'and' or 'or' at this time. Sequential 'then' will be added in a future update.
#' @param sequence Used to define if the segment should be 'in_order' (default), 'after', or 'before' the sequence of events
#' @param sequence_context Used to define the sequential items context which should be below the container context. ex. if container context is visitors then the sequence_context should be visits or hits
#' @param exclude Excludes the main container which will include all predicates. Only used when the predicate arguments are used.
#' @param version This is the default version. Only used if updating an existing segment. Not to be edited at this time.
#' @param rsid This is the report suite that the segment will be referenced to.
#' @param debug This enables the api call information to show in the console for help with debugging issues. default is FALSE
#' @param company_id This is the report suite that the segment will be referenced to.
#' @param client_id This is the report suite that the segment will be referenced to.
#' @param client_secret This is the report suite that the segment will be referenced to.
#'
#' @return A data frame of the new created segment id and other meta elements
#'
#' @import dplyr
#' @import assertthat
#' @import stringr
#' @importFrom glue glue
#' @export
#'
seg_build <- function(name = 'this is the name',
                      description = 'this is the description',
                      containers = NULL,
                      predicates = NULL,
                      sequences = NULL,
                      context = 'hits',
                      conjunction = 'and',
                      sequence = 'in_order',
                      sequence_context = 'hits',
                      exclude = FALSE, #only used if the 'predicates' argument is used
                      version = list(1, 0, 0),
                      debug = FALSE,
                      rsid = Sys.getenv('AW_REPORTSUITE_ID'),
                      company_id = Sys.getenv("AW_COMPANY_ID"),
                      client_id = Sys.getenv("AW_CLIENT_ID"),
                      client_secret = Sys.getenv("AW_CLIENT_SECRET")){

  if(is.null(containers) & !is.null(predicates) & is.null(sequences)) {
    if(exclude == FALSE) {
      if(length(predicates) == 1){
        if(!is.null(predicates[[1]]$val$`allocation-model`)) {
        if(context == 'visits' & predicates[[1]]$val$`allocation-model`$func == 'allocation-dedupedInstance'){
          predicates[[1]]$val$`allocation-model`$context = 'sessions'
          }
        }
        seg <- structure(list(
          name = name,
          description = description,
          definition = list(
            container = list(
              func = 'container',
              context = context,
              pred = predicates[[1]]
            ),
            func = 'segment',
            version = version
          ),
          rsid = rsid
        ))
      } else {
        seg <- structure(list(
          name = name,
          description = description,
          definition = list(
            container = list(
              func = 'container',
              context = context,
              pred = list(
                func = conjunction,
                preds = predicates
              )
            ),
            func = 'segment',
            version = version
          ),
          rsid = rsid
        ))
      }
    } #/exclude FALSE
    if(exclude == TRUE) {
      if(length(predicates) == 1) {
        seg <-  structure(list(
          name = name,
          description = description,
          definition = list(
            container = list(
              func = 'container',
              context = context,
              pred = list(
                func = 'without',
                pred = predicates[[1]]
                )
            ),
            func = 'segment',
            version = version
          ),
          rsid = rsid
        ))

      } else {
        seg <-  structure(list(
          name = name,
          description = description,
          definition = list(
            container = list(
              func = 'container',
              context = context,
              pred = list(
                func = 'without',
                pred = list(
                  func = 'container',
                  context = context,
                  pred = list(
                    func = conjunction,
                    preds = predicates
                  )
                )
              )
            ),
            func = 'segment',
            version = version
          ),
          rsid = rsid
        ))
      }
    } #/exclude TRUE
  } else if(is.null(predicates) & !is.null(containers) & is.null(sequences)){
    if(length(containers) == 1){
     seg <- structure(list(
       name = name,
       description = description,
        definition = list(
         container = list(
           func = 'container',
           context = context,
           pred = containers[[1]]
           ),
         func = 'segment',
         version = version
         ),
       rsid = rsid
       ))
      } else {
        seg <-  structure(list(
          name = name,
          description = description,
          definition = list(
            func = 'segment',
            version = version,
            container = list(
              func = 'container',
              context = context,
              pred = list(
                func = conjunction,
                preds = containers
              )
            )
          ),
          rsid = rsid
        ))
      }
  } else if(is.null(predicates) & is.null(containers) & !is.null(sequences)){
    sequence_dir <- dplyr::case_when(sequence == 'in_order' ~ 'sequence',
                                     sequence == 'after' ~ 'sequence-prefix',
                                     sequence == 'before' ~ 'sequence-suffix')

    ## Add in the necessary 'container' and 'hits' variables to each predicate for sequence to work
    seq_items <- list()
    for(i in seq_along(sequences)){
      if(!is.null(sequences[[i]]$val)) {
        seq_items[[i]] <- list(
          context = sequence_context,
          func = 'container',
          pred = sequences[[i]]
        )
      } else {
        seq_items[[i]] <- sequences[[i]]
      }
    }

    seg <- if(sequence_dir == 'sequence') {
      structure(list(
        name = name,
        description = description,
        definition = list(
          container = list(
            func = 'container',
            context = context,
            pred = list(
              func = sequence_dir,
              stream = seq_items
            )
          ),
          func = 'segment',
          version = version
        ),
        rsid = rsid
      ))
    } else if(sequence_dir %in% c('sequence-prefix', 'sequence-suffix')) {
      structure(list(
        name = name,
        description = description,
        definition = list(
          container = list(
            func = 'container',
            context = 'hits',
            pred = list(
              func = sequence_dir,
              context = context,
              stream = seq_items
            )
          ),
          func = 'segment',
          version = version
        ),
        rsid = rsid
      ))
    }
  } else if(is.null(predicates) & is.null(containers) & is.null(sequences)){
    stop('Either a predicate(s), containers, or sequences must be provided.')
  }
  #defined parts of the post request
  req_path = 'segments'

  body = seg

  #verify that the account has been authorized to make the post request
  token_config <- get_token_config(client_id = client_id, client_secret = client_secret)

  debug_call <- NULL

  if (debug) {
    debug_call <- httr::verbose(data_out = TRUE, data_in = TRUE, info = TRUE)
  }

  request_url <- sprintf("https://analytics.adobe.io/api/%s/%s?locale=en_US",
                         company_id, req_path)
  req <- httr::RETRY("POST",
                     url = request_url,
                     body = body,
                     encode = "json",
                     token_config,
                     debug_call,
                     httr::add_headers(
                       `x-api-key` = client_id,
                       `x-proxy-global-company-id` = company_id
                     ))

 dplyr::bind_rows(unlist(httr::content(req)))
}
