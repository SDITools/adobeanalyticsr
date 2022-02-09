#' Build the segment in Adobe Analytics
#'
#' This function combines predicates and/or containers and then makes the post call to create the segment in Adobe Analytics.
#'
#' @param name This is the name of the new segment (required)
#' @param description  This is the description of the segment (required)
#' @param containers List of the container(s) that make up the segment. Containers are list objects created using the `seg_con()` function.
#' @param predicates List of the predicate(s) to create a segment. Predicates are list objects created using the `seg_pred()` function.
#' @param sequences List of the predicate(s) and sequence container(s) that are combined to make a segment. Sequence containers are list objects created using the `seg_seq()` function.
#' @param context Defines the level that the segment logic should operate on. Valid values are visitors, visits, and hits. See Details
#' @param conjunction This will tell how the different containers and predicates should be compared. Use either 'and' or 'or'.
#' @param sequence Used to define if the segment should be 'in_order' (default), 'after', or 'before' the sequence of events
#' @param sequence_context Used to define the sequential items context which should be below the container context. ex. if container context is visitors then the sequence_context should be visits or hits
#' @param exclude Excludes the main container which will include all predicates. Only used when the predicate arguments are used.
#' @param create_seg Used to determine if the segment should be created in the report suite or if the definition should be returned to be used in a freeform table API call. Default is FALSE
#' @param rsid Adobe report suite ID (RSID).  If an environment variable called `AW_REPORTSUITE_ID` exists
#' in `.Renviron` or elsewhere and no `rsid` argument is provided, then the `AW_REPORTSUITE_ID` value will
#' be used. Use [aw_get_reportsuites()] to get a list of available `rsid` values.
#' @param debug This enables the api call information to show in the console for help with debugging issues. default is FALSE
#' @param company_id Company ID. If an environment variable called `AW_COMPANY_ID` exists in `.Renviron` or
#' elsewhere and no `company_id` argument is provided, then the `AW_COMPANY_ID` value will be used.
#' Use [get_me()] to get a list of available `company_id` values.
#' @param client_id Set in environment args, or pass directly here
#' @param client_secret Set in environment args, or pass directly here
#'
#' @details
#'
#' **Context**
#' The rules in a segment have a context that specify the level of operation. The context can be visitors, visits or hits.
#' As an example, let's build a segment rule where revenue is greater than 0 (meaning a purchase took place) and change the context to see how things change.
#' If the context is set to visitors, the segment includes all hits from visitors that have a purchase of some kind during a visit. This is useful in analyzing customer behavior in visits leading up to a purchase and possibly behavior after a purchase.
#' the context is set to visits, the segment includes all hits from visits where a purchase occurred. This is useful for seeing the behavior of a visitor in immediate page views leading up to the purchase.
#' If the context is set to hit, the segment only includes hits where a purchase occurred, and no other hits. This is useful in seeing which products were most popular.
#' In the above example, the context for the container listed is hits. This means that the container only evaluates data at the hit level, (in contrast to visit or visitor level). The rows in the container are also at the hit level.
#'
#' @return If the segment validates it will return a data frame of the newly created segment id along with some other basic meta data. If it returns and error then the error
#' response will be returned to help understand what needs to be corrected.
#'
#' @import dplyr
#' @import assertthat
#' @import stringr
#' @importFrom glue glue
#' @export
#'
seg_build <- function(name = NULL,
                      description = NULL,
                      containers = NULL,
                      predicates = NULL,
                      sequences = NULL,
                      context = 'hits',
                      conjunction = 'and',
                      sequence = 'in_order',
                      sequence_context = 'hits',
                      exclude = FALSE,
                      create_seg = FALSE,
                      debug = FALSE,
                      rsid = Sys.getenv('AW_REPORTSUITE_ID'),
                      company_id = Sys.getenv("AW_COMPANY_ID"),
                      client_id = Sys.getenv("AW_CLIENT_ID"),
                      client_secret = Sys.getenv("AW_CLIENT_SECRET")){
  #validate arguments
  if(is.null(name) || is.null(description)) {
    stop('The arguments `name` and `description` must be included.')
  }

  #define the new segment version
  version <- list(0, 1, 0)

  #Create the segment list object
  if (!is.null(predicates) && is.null(containers) && is.null(sequences)) { ## Predicates
    if (exclude == FALSE) {
      if (length(predicates) == 1){
        if (!is.null(predicates[[1]]$val$`allocation-model`)) {
        if (context == 'visits' && predicates[[1]]$val$`allocation-model`$func == 'allocation-dedupedInstance'){
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
    if (exclude == TRUE) {
      if (length(predicates) == 1) {
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
  } else if (is.null(predicates) && !is.null(containers) && is.null(sequences)){  #Containers
    if (length(containers) == 1) {
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
  } else if(is.null(predicates) && is.null(containers) && !is.null(sequences)) {
    sequence_dir <- dplyr::case_when(sequence == 'in_order' ~ 'sequence',
                                     sequence == 'after' ~ 'sequence-prefix',
                                     sequence == 'before' ~ 'sequence-suffix')

    ## Add in the necessary 'container' and 'hits' variables to each predicate for the sequence to work
    seq_items <- list()
    for (i in seq_along(sequences)) {
      if (!is.null(sequences[[i]]$stream)) {
        seq_items[[i]] <- list(
          context = sequence_context,
          func = 'container',
          pred = sequences[[i]]
        )
      } else if (!is.null(sequences[[i]]$val)) {
        seq_items[[i]] <- list(
          context = sequence_context,
          func = 'container',
          pred = sequences[[i]]
        )
      } else {
        seq_items[[i]] <- sequences[[i]]
      }
    }

    seg <- if (sequence_dir == 'sequence') {
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
    } else if (sequence_dir %in% c('sequence-prefix', 'sequence-suffix')) {
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
  } else if (is.null(predicates) & is.null(containers) & is.null(sequences)) {
    stop('Either a predicate(s), containers, or sequences must be provided.')
  }
  #defined parts of the post request
  req_path <- 'segments'

  body <- seg

  if (!create_seg) {
    jsonlite::toJSON(body, auto_unbox = T)
  } else if (!create_seg) {
    req <- aw_call_data(req_path = req_path, body = seg,
                        company_id = company_id,
                        client_id = client_id,
                        client_secret = client_secret)
    req
  }

}
