#' Retrieve a list of verbs for segments
#'
#' This function generates a list of linking verbs that can be used to build segments
#'
#' @return A dataframe of all available segment verbs
#'
#' @examples
#' seg_verbs()
#'
#' @export
#'
seg_verbs <- function(){
  verbs <- data.frame(
    Type = c(
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "string",
      "number",
      "number",
      "number",
      "number",
      "number",
      "number",
      "number",
      "number",
      "exists",
      "exists"
    ),
    Function = c(
      'streq',
      'not-streq',
      'strlt',
      'strgt',
      'strle',
      'strge',
      'streq-in',
      'not-streq-in',
      'contains',
      'not-contains',
      'contains-any-of',
      'contains-all-of',
      'not-contains-any-of',
      'not-contains-all-of',
      'starts-with',
      'ends-with',
      'not-starts-with',
      'not-ends-with',
      'matches',
      'not-matches',
      'eq',
      'not-eq',
      'gt',
      'lt',
      'ge',
      'le',
      'eq-any-of',
      'not-eq-any-of',
      'exists',
      'not-exists'),
    Description = c(
      "Equals",
      "Not Equals",
      "Less Than",
      "Greater Than",
      "Less Than or Equals",
      "Greater Than or Equals",
      "Match a string to any of the values in the parameter",
      "Ensure a string doesn't match any of the values in the parameter",
      "Ensure a string matches or contains the value in the parameter",
      "Ensure a string doesn't match or contains the value in the parameter",
      "Ensure a string contains any of the values in the parameter. Case-insensitive.",
      "Ensure a string contains all of the values in the parameter. Case-insensitive.",
      "Ensure a string doesn't contain at least one of the values in the parameter. Case-insensitive.",
      "Ensure a string doesn't contain any of the values in the parameter. Case-insensitive.",
      "Ensure a string starts with the value in the parameter. Case-insensitive.",
      "Ensure a string ends with the value in the parameter. Case-insensitive.",
      "Ensure a string doesn't start with the value in the parameter. Case-insensitive.",
      "Ensure a string doesn't end with the value in the parameter. Case-insensitive.",
      "Ensure a string matches the glob parameter. A glob parameter uses a '' character to match any sequence of characters. A literal '' is expressed with '*'.",
      "Ensure a string doesn't match the glob parameter. A glob parameter uses a '' character to match any sequence of characters. A literal '' is expressed with '*'.",
      "Equals",
      "Not equals",
      "Greater than",
      "Less than",
      "Greater than or equal to",
      "Less than",
      "Equal to any of the values provided",
      "Not equal to any of the values provided",
      "Tests if an attribute has been set to a value.",
      "Tests if an attribute has never been set to a value."
    )
  )
  return(verbs)
}
