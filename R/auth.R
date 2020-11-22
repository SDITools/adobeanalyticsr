#' OAuth2 Token for Adobe Analytics
#'
#'
#' @param client_id defined by global variable or manually defined
#' @param client_secret defined by global variable or manually defined
#'
#' @export
#' @import httr
#'
aw_token <- function(client_id = Sys.getenv("AW_CLIENT_ID"),
                     client_secret = Sys.getenv("AW_CLIENT_SECRET")){

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
    use_oob = TRUE,
    oob_value = "https://adobeanalyticsr.com/token/result.html"
  )
}
