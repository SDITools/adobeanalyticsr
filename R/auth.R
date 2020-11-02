#' OAuth2 Token for Adobe Analytics
#'
#'
#' @param client_id defined by global variable or manually defined
#' @param client_secret defined by global variable or manually defined
#'
#' @export
#' @import httr
#'
aa_token <- function(client_id = Sys.getenv("AA_CLIENT_ID"),
                     client_secret = Sys.getenv("AA_CLIENT_SECRET")){

  # https://ims-na1.adobelogin.com/ims/authorize?client_id={CLIENT ID}&redirect_uri={REDIRECT URI}&scope=openid,
  # AdobeID,read_organizations,additional_info.job_function,additional_info.projectedProductContext&response_type=code

  aa_endpoint <- httr::oauth_endpoint(
    authorize = "authorize/",
    access = "token/v1",
    base_url = "https://ims-na1.adobelogin.com/ims/"
  )

  aa_app <- httr::oauth_app(
    appname = "adobe_analytics_v2.0",
    key = client_id,
    secret = client_secret
  )

  #Oauth2 token
  httr::oauth2.0_token(
    endpoint = aa_endpoint,
    app = aa_app,
    scope = "openid,AdobeID,read_organizations,additional_info.projectedProductContext,additional_info.job_function",
    cache = "aa.oauth",
    use_oob = TRUE,
    oob_value = "https://adobeanalyticsr.com/token/result.html"
  )
}

