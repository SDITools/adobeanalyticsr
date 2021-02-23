# adobeanalyticsr 0.1.5

* Adjusted a few minor things in prep for CRAN submission

# adobeanalyticsr 0.1.4

* Fixed issue with non-numeric metrics returned. Specifically 'Infinite' is being returned for some calculated metrics

# adobeanalyticsr 0.1.3

* Changed function get_usage() --> get_usage_log() to better represent the endpoint
* Added Getting Started Vignette

# adobeanalyticsr 0.1.2

* New get_usage() function that returns the usage and access logs for a given date range within a 3 month period.

# adobeanalyticsr 0.1.1

* New get_users() function that retrieves a list of all users for the company designated by the auth token.
* Now includes catch for date_range end-date character value error

# adobeanalyticsr 0.1.0

* First public version
* Added a `NEWS.md` file to track changes to the package.
