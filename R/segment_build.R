#' Build the Segment
#'
#' This function combines predicates into a container.
#'
#' @param name This is the name of the new segment
#' @param description  This is the description of the segment
#' @param containers List of the container(s) that make up the segment
#' @param predicates List of the predicate(s) to create a segment
#' @param context This defines the main container context, either hits, visits (Default), or visitors
#' @param conjunction This will tell how the different containers and predicates should be compared. Use either 'and' or 'or' at this time. Sequential 'then' will be added in a future update.
#' @param exclude excludes the main container which will include all predicates. Only used when the predicate arguments are used.
#' @param version This is the default version. Only used if updating an existing segment. Not to be edited at this time.
#' @param rsid This is the report suite that the segment will be referenced to.
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
                      context = 'hits',
                      conjunction = 'and',
                      exclude = FALSE, #only used if the 'predicates' argument is used
                      version = list(1, 0, 0),
                      rsid = Sys.getenv('AW_REPORTSUITE_ID'),
                      company_id = Sys.getenv("AW_COMPANY_ID"),
                      client_id = Sys.getenv("AW_CLIENT_ID"),
                      client_secret = Sys.getenv("AW_CLIENT_SECRET")){
  if(is.null(containers) & !is.null(predicates)) {
    if(exclude == FALSE) {
      if(length(predicates) == 1){
        seg <- structure(list(
          name = name,
          description = description,
          definition = list(
            container = list(
              func = 'container',
              context = context,
              pred = list(
                  func = 'container',
                  context = context,
                  pred = predicates[[1]]
              )
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
                pred = list(
                  func = 'container',
                  context = context,
                  pred = predicates[[1]]
                  )
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
  } else if(is.null(predicates) & !is.null(containers)){
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
  } else if(is.null(predicates) & is.null(containers)){
    stop('Either a predicate(s) or containers must be provided.')
  }
  #defined parts of the post request
  req_path = 'segments'
  body = seg

  #verify that the account has been authorized to make the post request
  token <- aw_token(client_id, client_secret)

  request_url <- sprintf("https://analytics.adobe.io/api/%s/%s?locale=en_US",
                         company_id, req_path)
  req <- httr::RETRY("POST",
                     url = request_url,
                     body = body,
                     encode = "json",
                     #httr::config(token = token),
                     httr::verbose(data_out = TRUE),
                     httr::add_headers(
                       `x-api-key` = client_id,
                       `x-proxy-global-company-id` = company_id
                     ))

 dplyr::bind_rows(unlist(httr::content(req)))
}
