#' `adobeanalyticsr` Package
#'
#' Connect to the 'Adobe Analytics' API v2.0
#' <https://github.com/AdobeDocs/analytics-2.0-apis> which powers 'Analysis
#' Workspace'. The package was developed with the analyst in mind, and it will
#' continue to be developed with the guiding principles of iterative,
#' repeatable, timely analysis.
#'
#'
"_PACKAGE"


# Environment for caching tokens for the session
.adobeanalytics <- new.env(parent = emptyenv())