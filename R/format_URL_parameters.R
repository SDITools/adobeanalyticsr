#' Format list as query parameters
#'
#' Pretty much ripped straight from `httr`, but it's not exported there so I
#' had to do this.
#'
#' I'm going to add some functionality here to automatically collapse vector
#' inputs into a single comma-separated string. This is used in probably all
#' cases. In cases where it isn't used, the programmer can pre-collapse the
#' input to prevent automatic conversion to a comma-separated string.
#'
#' But that raises a question: how should `NA`s and `NULL`s be handled? The case
#' will be this:
#'
#' - All `NA`s is not allowed
#' - `NULL` is omitted by `purrr::compact`
#' - Some `NA`s will be converted to the string "NA" and passed onto the API
#'
#' @param elements Named list of query parameters
#'
#' @return String, `x` formatted as a parameter list
#' @noRd
format_URL_parameters <- function(elements) {
  if (length(elements) == 0 | purrr::every(elements, is.null)) {
    return("")
  }

  stopifnot(is.list(elements))

  if (!is_fully_named_list(elements)) {
    stop("All components of query must be named", call. = FALSE)
  }

  elements <- purrr::compact(elements)
  names <- utils::URLencode(names(elements), reserved = TRUE)

  comma_collapse_elements <- function(x) {
    # All NAs is considered an error (may be temporary)
    assertthat::assert_that(!all(is.na(x)))

    out <- paste(x, collapse = ",")

    if (!inherits(x, "AsIs")) {
      out <- utils::URLencode(out, reserved = TRUE)
    }

    out
  }


  values <- vapply(elements, comma_collapse_elements, character(1))

  paste0(names, "=", values, collapse = "&")
}



is_fully_named_list <- function(l) {
  if (length(names(l)[names(l) != ""]) != length(l)) {
    FALSE
  } else {
    TRUE
  }
}