#' Generate an access token for Adobe Analytics 2.0
#'
#' `aw_oauth` and `aw_jwt` should not be called directly, as these do not cache
#' the token.
#'
#' @param type Either 'jwt' or 'oauth'. Defaults to 'oauth'.
#' @param ... Arguments passed to auth functions.
#' @param client_id Client ID
#' @param client_secret Client secret
#' @param private_key File path to private key for token signature
#' @param org_id Organization ID from integration console
#' @param tech_id Technical account ID from integration console
#' @param jwt_token Optional, a custom, encoded, signed JWT claim. If used,
#'   only `client_id` and `client_secret` are required.
#' @param use_oob if `FALSE`, use a local webserver for the OAuth dance.
#'   Otherwise, provide a URL to the user and prompt for a validation code.
#'   Defaults to the value of the `httr_oob_default` default, or TRUE if
#'   `httpuv` is not installed.
#'
#' @return The path of the cached token, invisibly
#' @family auth
#' @aliases aw_auth auth_jwt auth_oauth
aw_auth <- function(type = "oauth", ...) {
    # Type will be null if you pass .adobeanalyticsr$type without authenticating first
    # Hence the circular error message
    if (is.null(type)) {
        stop("Authentication type missing, please authenticate with `aw_auth`")
    }
    type <- match.arg(type, c("jwt", "oauth"))

    switch(type,
        jwt = auth_jwt(...),
        oauth = auth_oauth(...)
    )

    stash_token()
}


#' Specify the type of auth for the session
#'
#' @param type Either "oauth" or "jwt"
#'
#' @return `type`, invisibly
aw_auth_with <- function(type) {
    type <- match.arg(type, c("oauth", "jwt"))

    .adobeanalytics$type <- type
    invisible(type)
}


#' Retrieve a token
#'
#' Updates (if necessary) and returns session token. First checks for a session
#' token, then a cached token, then generates a new token. The type may
#' be set for the session with `aw_auth_with`. It is also automatically set when
#' any auth function is executed.
#'
#' The `type` argument can be used to override the precedence of the tokens, but
#' this will throw a warning. If there is no session token, the token returned
#' by `retrieve_aw_token()` is guaranteed to be of type `type`. However, this
#' is not the intended use of this function (use `aw_auth` instead), so a
#' warning is thrown.
#'
#' @param type Type of auth to use if there is no session token or cached token.
#'   One of "oauth" or "jwt".
#' @param ... Further arguments passed to auth functions
#'
#' @importFrom rlang %||%
#'
#' @return A token object of type `response` (JWT) or `Token2.0` (OAuth)
retrieve_aw_token <- function(type = NULL, ...) {
    stopifnot(is.character(type) || is.null(type))

    # Check session token
    token <- .adobeanalytics$token
    type <- type %||% .adobeanalytics$type

    if (!is.null(token) & !is.null(type)) {
        if (type != token_type(token)) {
            stop("Token type mismatch, malformed session token/type relationship")
        }
    }

    # Session token > cached token > generating new token
    if (is.null(token)) {
        path <- token_path(getOption("aw_auth_name", "aw_auth.rds"))
        cached_token_exists <- file.exists(path)

        if (cached_token_exists) {
            message(paste("Retrieving cached token:", path))
            token <- readRDS(path)
            type <- type %||% token_type(token)

            if (type != token_type(token)) {
                warning("Type mismatch when fetching cached token -- did you mean to call aw_auth() instead?\n--> Calling aw_auth to override cached token")
                aw_auth(type, ...)
                token <- .adobeanalytics$token
                type <- .adobeanalytics$type
            }

            .adobeanalytics$token <- token
            .adobeanalytics$type <- type
        } else {
            message("No session token or cached token -- generating new token")
            aw_auth(type = type, ...)
        }
    }

    # Check expiration for cached token
    if (type == "oauth") {
        # OAuth 2.0 refresh takes care of itself
        # Note: there might not be a way to use the refresh token
        aw_auth(type = "oauth", ...)
    } else if (type == "jwt") {
        # JWT can easily be regenerated
        if (is_jwt_expired(token)) aw_auth(type = "jwt", ...)
    }

    return(.adobeanalytics$token)
}


#' Cache token
#'
#' The token is saved as `aw_auth.rds` in `rappdirs::user_data_dir("adobeanalyticsr", "R")`
#' The `"aw_auth_name"` option can be used to override the default filename.
#' In a pinch, this could be used to store multiple tokens.
#'
#' @return Invisibly, the filename of the token
stash_token <- function() {
    if(is.null(.adobeanalytics$token)) stop("Token not set correctly")

    path <- token_path(getOption("aw_auth_name", "aw_auth.rds"))

    message(paste0("Saving token to '", path, "'"))
    dir.create(token_path(), showWarnings = FALSE, recursive = TRUE)
    saveRDS(.adobeanalytics$token, path)

    invisible(path)
}


#' Standard location for token caching
#'
#' @param ... Passed to file.path, usually a filename
#'
#' @return File path
token_path <- function(...) {
    loc <- rappdirs::user_data_dir("adobeanalyticsr", "R")
    file.path(loc, ...)
}

#' Get type of token
#'
#' In the future, this could become a custom object, but for now there
#' are too many differences between the token types, so I just check for
#' each class.
#'
#' @param token An `httr` `reponse` object or `oauth2.0_token` object
#'
#' @return Either 'oauth' or 'jwt'
token_type <- function(token) {
    if (inherits(token, "Token2.0")) {
        "oauth"
    } else if (inherits(token, "response")) {
        "jwt"
    } else if (is.null(token)) {
        NULL
    } else {
        stop("Unknown token type")
    }
}


# JWT ------------------------------------------------------------------

#' @family auth
#' @describeIn aw_auth Authenticate with JWT token
#' @export
auth_jwt <- function(client_id = Sys.getenv("AW_CLIENT_ID"),
                     client_secret = Sys.getenv("AW_CLIENT_SECRET"),
                     private_key = Sys.getenv("AW_PRIVATE_KEY"),
                     org_id = Sys.getenv("AW_ORGANIZATION_ID"),
                     tech_id = Sys.getenv("AW_TECHNICAL_ID"),
                     jwt_token = NULL,
                     ...) {
    stopifnot(is.character(client_id))
    stopifnot(is.character(client_secret))
    stopifnot(is.character(private_key))
    stopifnot(is.character(org_id))
    stopifnot(is.character(tech_id))

    if (any(c(client_id, client_secret) == "")) {
        stop("Client ID or Client Secret not found. Are your environment variables named `AW_CLIENT_ID` and `AW_CLIENT_SECRET`?")
    }


    jwt_token <- get_jwt_token(jwt_token = jwt_token,
                               client_id = client_id,
                               private_key = private_key,
                               org_id = org_id,
                               tech_id = tech_id)


    token <- httr::POST(url="https://ims-na1.adobelogin.com/ims/exchange/jwt",
                        body = list(
                            client_id = client_id,
                            client_secret = client_secret,
                            jwt_token = jwt_token
                        ),
                        encode = 'form')

    httr::stop_for_status(token)

    # If successful
    message("Successfully authenticated with JWT: access token valid until ",
                  token$date + httr::content(token)$expires_in / 1000)

    .adobeanalytics$token <- token
    .adobeanalytics$type <- "jwt"
}


#' Get an encoded, signed JWT token
#'
#' Gets a JWT token
#'
#' @param jwt_token Optional, a JWT token (e.g., a cached token)
#' @param client_id Client ID
#' @param private_key File path to private key for token signature
#' @param org_id Organization ID from integration console
#' @param tech_id Technical account ID from integration console
#'
#' @return A JWT token generated by [jose::jwt_encode_sig()]
get_jwt_token <- function(jwt_token = NULL,
                          client_id,
                          private_key,
                          org_id,
                          tech_id) {
    if (is.null(jwt_token)) {
        if (any(c(org_id, tech_id, private_key) == "")) {
            stop("Missing one of org_id, tech_id, or private_key")
        }

        if (!(inherits(private_key, "key") || file.exists(private_key))) {
            stop("Invalid private key. Is private key a file or the result of `openssl::read_key`?")
        }

        jwt_claim <- jose::jwt_claim(
            exp = as.integer(as.POSIXct(Sys.time() + as.difftime(1, units = "mins"))),
            iss = org_id,
            sub = tech_id,
            aud = paste0('https://ims-na1.adobelogin.com/c/', client_id),
            # Metascope for Adobe Analytics
            "https://ims-na1.adobelogin.com/s/ent_analytics_bulk_ingest_sdk" = TRUE,
            iat = NULL
        )
        jwt_token <- jose::jwt_encode_sig(jwt_claim, private_key, size = 256)
    }

    jwt_token
}


#' Check whether token expired (internal)
#'
#' @param token The access token to check
#'
#' @return TRUE or FALSE
is_jwt_expired <- function(token) {
    # allow 20 mins grace for long calls
    token$date + httr::content(token)$expires_in / 1000 <= Sys.time() - 1200
}



# OAuth ----------------------------------------------------------------
#' @family auth
#' @describeIn aw_auth Authorize via OAuth 2.0
#' @export
auth_oauth <- function(client_id = Sys.getenv("AW_CLIENT_ID"),
                       client_secret = Sys.getenv("AW_CLIENT_SECRET"),
                       use_oob = TRUE) {
    stopifnot(is.character(client_id))
    stopifnot(is.character(client_secret))

    if (any(c(client_id, client_secret) == "")) {
        stop("Client ID or Client Secret not found. Are your environment variables named `AW_CLIENT_ID` and `AW_CLIENT_SECRET`?")
    }

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
    token <- httr::oauth2.0_token(
        endpoint = aw_endpoint,
        app = aw_app,
        scope = "openid,AdobeID,read_organizations,additional_info.projectedProductContext,additional_info.job_function",
        cache = "aa.oauth",
        use_oob = use_oob,
        oob_value = ifelse(use_oob, "https://adobeanalyticsr.com/token_result.html", NULL)
    )

    message("Successfully authenticated with OAuth")
    .adobeanalytics$token <- token
    .adobeanalytics$type <- "oauth"
}
