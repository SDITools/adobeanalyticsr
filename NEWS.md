# adobeanalyticsr 0.2.0

* Added support for JWT Authentication
* Changed authentication process with new functions 
  * `aw_auth()` 
  * `aw_auth_with()`
  * `aw_auth_name()`
  * `aw_auth_path()`
* Added support for `aw_token()` function with life-cycle warning
* Added new function called `aw_segment_table()` which returns one or multiple segments and their corresponding metrics
* Cleanup and edits to the `aw_freeform_table()` function
* Token support updates for all api call functions.
* Removed `client_id` and `client_secret` from functions that were not using those variables even if defined in the arguments.
* Fixed a few bugs around api calls that returned NA 
* Added argument to the `aw_freeform_table()` function that will prevent metric and dimension validation check to run at the start of every function call. This will save users a small amount of time but could be significant in the long run.
* Added progress bar to `aw_freeform_table()` and re-calibrated the estimated finish time.
* Enhanced authorization retention by saving client ID and client secret in the session environment
* Killed global counter on short requests
* Removed the search clause restriction to enable "CONTAINS" to be understood as the default for `aw_freeform_report()` search arguments.
* Added search argument to recursive call
* Updated date_range argument to handle datetimes better

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
