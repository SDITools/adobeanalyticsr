# This function generates a list of linking verbs that can be used to build segments
seg_verbs <- data.frame(rbind(
  c( "string", 'string','streq', "Equals"),
  c( "string", 'string', 'not-streq', "Not Equals"),
  c( "string", 'string', 'strlt', "Less Than"),
  c( "string", 'string', 'strgt', "Greater Than"),
  c( "string", 'string', 'strle', "Less Than or Equals"),
  c( "string", 'string', 'strge', "Greater Than or Equals"),
  c( "string", 'list', 'streq-in',  "Match a string to any of the values in the parameter"),
  c( "string", 'list', 'not-streq-in', "Ensure a string doesn't match any of the values in the parameter"),
  c( "string", 'string', 'contains', "Ensure a string matches or contains the value in the parameter"),
  c( "string", 'string', 'not-contains', "Ensure a string doesn't match or contains the value in the parameter"),
  c( "string", 'list', 'contains-any-of', "Ensure a string contains any of the values in the parameter. Case-insensitive."),
  c( "string", 'list', 'contains-all-of', "Ensure a string contains all of the values in the parameter. Case-insensitive."),
  c( "string", 'list', 'not-contains-any-of', "Ensure a string doesn't contain at least one of the values in the parameter. Case-insensitive."),
  c( "string", 'list', 'not-contains-all-of', "Ensure a string doesn't contain any of the values in the parameter. Case-insensitive."),
  c( "string", 'string', 'starts-with', "Ensure a string starts with the value in the parameter. Case-insensitive."),
  c( "string", 'string', 'ends-with', "Ensure a string ends with the value in the parameter. Case-insensitive."),
  c( "string", 'string', 'not-starts-with', "Ensure a string doesn't start with the value in the parameter. Case-insensitive."),
  c( "string", 'string', 'not-ends-with', "Ensure a string doesn't end with the value in the parameter. Case-insensitive."),
  c( "string", 'glob', 'matches', "Ensure a string matches the glob parameter. A glob parameter uses a '' character to match any sequence of characters. A literal '' is expressed with '*'."),
  c( "string", 'glob', 'not-matches', "Ensure a string doesn't match the glob parameter. A glob parameter uses a '' character to match any sequence of characters. A literal '' is expressed with '*'."),
  c( "number", 'number', 'eq', "Equals"),
  c( "number", 'number', 'not-eq', "Not equals"),
  c( "number", 'number', 'gt', "Greater than"),
  c( "number", 'number', 'lt', "Less than"),
  c( "number", 'number', 'ge', "Greater than or equal to"),
  c( "number", 'number', 'le', "Less than"),
  c( "number", 'list', 'eq-any-of', "Equal to any of the values provided"),
  c( "number", 'list', 'not-eq-any-of', "Not equal to any of the values provided"),
  c( "number", 'list', 'eq-in', "Equal to any of the values provided"),
  c( "number", 'list', 'not-eq-in', "Not equal to any of the values provided"),
  c( "string", 'exists', 'exists', "Tests if an attribute has been set to a value."),
  c( "string", 'exists','not-exists', "Tests if an attribute has never been set to a value."),
  c( "number", 'exists','event-exists', "Tests if an attribute has been set to a number value."),
  c( "number", 'exists','not-event-exists', "Tests if an attribute has never been set to a number value.")
))

colnames(seg_verbs) <-  c('type', 'class', 'verb', 'description')

seg_verbs <- seg_verbs %>%
  mutate(
    arg = case_when(
      class == "exists" ~ NA_character_,
      class == "string" ~ "str",
      class == "list" ~ "list",
      class == "number" ~ "num",
      class == "glob" ~ "glob",
      TRUE ~ NA_character_
    )
  )

readr::write_csv(seg_verbs, "data-raw/seg_verbs.csv")
readr::write_csv(seg_verbs, "inst/extdata/seg_verbs.csv")
usethis::use_data(seg_verbs, compress = "xz", overwrite = T)
