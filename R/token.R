#' OAuth2 Token for Adobe Analytics
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param client_id defined by global variable or manually defined
#' @param client_secret defined by global variable or manually defined
#' @param use_oob for the purpose of testing. Default is set to TRUE
#'
#' @return An authorization token is saved the file name aa.oauth. If the file aa.oauth does not exist then one will be created at the end of the authorization process.
#'
#' @export
#' @import httr
#'
aw_token <- function(client_id = Sys.getenv("AW_CLIENT_ID"),
                     client_secret = Sys.getenv("AW_CLIENT_SECRET"),
                     use_oob = TRUE){
  lifecycle::deprecate_warn(when = "0.1.6", what = "aw_token()", with = "aw_auth()")

  token <- old_aw_token(client_id = client_id, client_secret = client_secret, use_oob = use_oob)
  .adobeanalytics$token <- token
  token
}


#' Old aw_token function
#'
#' This is the old aw_token function, which is deprecated in the new version
#' of the package. It will still work, but `aw_auth` is the recommended way to
#' auth now.
#'
#' @inheritParams aw_token
#'
#' @noRd
old_aw_token <- function(client_id = Sys.getenv("AW_CLIENT_ID"),
                         client_secret = Sys.getenv("AW_CLIENT_SECRET"),
                         use_oob = TRUE) {
  aw_endpoint <- httr::oauth_endpoint(
    authorize = "authorize/v2/",
    access = "token/v3",
    base_url = "https://ims-na1.adobelogin.com/ims/"
  )

  aw_app <- httr::oauth_app(
    appname = "adobe_analytics_v2.0",
    key = client_id,
    secret = client_secret
  )

  #Oauth2 token
  httr::oauth2.0_token(
    endpoint = aw_endpoint,
    app = aw_app,
    scope = "openid,AdobeID,read_organizations,additional_info.projectedProductContext,additional_info.job_function",
    cache = "aa.oauth",
    use_oob = use_oob,
    oob_value = "https://adobeanalyticsr.com/token_result.html"
  )
}