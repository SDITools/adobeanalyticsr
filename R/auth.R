# General auth -------------------------------------------------

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
#' @export
aw_auth <- function(type = aw_auth_with(), ...) {
    if (is.null(type)) {
        stop("Authentication type missing, please set an auth type with `aw_auth_with`")
    }
    type <- match.arg(type, c("jwt", "oauth"))

    switch(type,
        jwt = auth_jwt(...),
        oauth = auth_oauth(...)
    )
}


#' Set authentication options
#'
#' @description
#' If get or set an auth option. If called without an argument, gets the current
#' setting for the requested option. Pass `NULL` as an argument to clear the
#' setting and return to defaults.
#'
#' `aw_auth_with` sets the type of authorization for the session. This is used
#' as a default when no specific option is given.
#'
#' @param type Either "oauth" or "jwt"
#' @param path Location for the auth file. If the location does not exist,
#'   it will be created the first time a token is cached.
#' @param name Filename, such as `aw_auth.rds`. The file is stored as an RDS
#'   file, but there is no requirement for the `.rds` file extension. `.rds` is
#'   not appended automatically.
#'
#' @return The option value, invisibly
#' @family options
#' @rdname aw_auth_with
#' @aliases aw_auth_with aw_auth_path aw_auth_name
#' @export
aw_auth_with <- function(type) {
    if (missing(type)) return(getOption("adobeanalyticsr.auth_type"))

    if (!is.null(type)) {
        type <- match.arg(type, c("oauth", "jwt"))
    }

    options(adobeanalyticsr.auth_type = type)
    invisible(type)
}


#' @description
#' `aw_auth_path` sets the file path for the cached authorization token. It
#' should be a directoy, rather than a filename. If this option is not set, the
#' current working directory is used instead.
#'
#' @rdname aw_auth_with
#' @family options
#' @export
aw_auth_path <- function(path) {
    if (missing(path)) return(getOption("adobeanalyticsr.auth_path"))
    options(adobeanalyticsr.auth_path = path)
    invisible(path)
}


#' @description
#' `aw_auth_name` sets the file name for the cached authorization token. If this
#' option is not set, the default filename is `aw_auth.rds`
#'
#' @rdname aw_auth_with
#' @family options
#' @export
aw_auth_name <- function(name) {
    if (missing(name)) return(getOption("adobeanalyticsr.auth_name"))
    options(adobeanalyticsr.auth_name = name)
    invisible(name)
}



#' Retrieve a token
#'
#' Updates (if necessary) and returns session token. First checks for a session
#' token, then a cached token, then generates a new token. The default type may
#' be set for the session with `aw_auth_with`.
#'
#' @param ... Further arguments passed to auth functions
#'
#' @importFrom rlang %||%
#' @keywords internal
#' @return A token object of type `response` (JWT) or `Token2.0` (OAuth)
retrieve_aw_token <- function(...) {
    # Check session token
    token <- .adobeanalytics$token
    type <- token_type(token) %||% aw_auth_with()

    if (!is.null(token) & !is.null(type)) {
        if (type != token_type(token)) {
            stop("Token type mismatch, malformed session token/type relationship")
        }
    }

    # Session token > cached token > generating new token
    if (is.null(token)) {
        path <- token_path(getOption("adobeanalyticsr.auth_name", "aw_auth.rds"))
        cached_token_exists <- file.exists(path)

        if (cached_token_exists && type == "oauth") {
            message(paste("Retrieving cached token:", path))
            token <- readRDS(path)
            type <- token_type(token)

            .adobeanalytics$token <- token
        } else {
            message("No session token or cached token -- generating new token")
            aw_auth(type = aw_auth_with(), ...)
            token <- .adobeanalytics$token
            type <- aw_auth_with()
        }
    }

    # Check expiration
    if (!token$validate()) {
        # This might be the wrong thing to do with OAuth, but it's the right
        # thing to do for JWT
        .adobeanalytics$token$refresh()
    }

    return(.adobeanalytics$token)
}


#' Standard location for token caching
#'
#' The default path for the token is the current working directory, but
#' the option `adobeanalyticsr.auth_path` overrides this behavior.
#'
#' @param ... Passed to file.path, usually a filename
#'
#' @return File path
token_path <- function(...) {
    loc <- getOption("adobeanalyticsr.auth_path", getwd())
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
#' @export
token_type <- function(token) {
    if (inherits(token, "Token2.0")) {
        "oauth"
    } else if (inherits(token, "AdobeJwtToken")) {
        "jwt"
    } else if (is.null(token)) {
        NULL
    } else {
        stop("Unknown token type")
    }
}


#' Get token configuration for requests
#'
#' Returns a configuration for `httr::GET` for the correct token type.
#'
#' @param client_id Client ID
#' @param client_secret Client secret
#'
#' @return Config objects that can be passed to `httr::GET` or similar
#' functions (e.g. `httr::RETRY`)
get_token_config <- function(client_id,
                             client_secret) {
    token <- retrieve_aw_token(client_id,
                               client_secret)
    type <- token_type(token)

    switch(type,
        oauth = httr::config(token = token),
        jwt = httr::add_headers(Authorization = paste("Bearer", content(token$token)$access_token)),
        stop("Unknown token type")
    )
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
    secrets <- list(
        client_id = client_id,
        client_secret = client_secret,
        private_key = private_key,
        org_id = org_id,
        tech_id = tech_id
    )

    resp <- auth_jwt_gen(secrets = secrets, jwt_token = jwt_token)


    # If successful
    message("Successfully authenticated with JWT: access token valid until ",
                  resp$date + httr::content(resp)$expires_in / 1000)

    .adobeanalytics$token <- AdobeJwtToken$new(resp, secrets)
}


#' Generate the authentication response object
#'
#' @param secrets List of secret values, see `auth_jwt`
#' @param jwt_token Optional, a JWT token (e.g., a cached token)
#'
#' @noRd
auth_jwt_gen <- function(secrets,
                         jwt_token = NULL) {

    stopifnot(is.character(secrets$client_id))
    stopifnot(is.character(secrets$client_secret))
    stopifnot(is.character(secrets$private_key))
    stopifnot(is.character(secrets$org_id))
    stopifnot(is.character(secrets$tech_id))

    if (any(c(secrets$client_id, secrets$client_secret) == "")) {
        stop("Client ID or Client Secret not found. Are your environment variables named `AW_CLIENT_ID` and `AW_CLIENT_SECRET`?")
    }


    jwt_token <- get_jwt_token(jwt_token = jwt_token,
                               client_id = secrets$client_id,
                               private_key = secrets$private_key,
                               org_id = secrets$org_id,
                               tech_id = secrets$tech_id)


    token <- httr::POST(url="https://ims-na1.adobelogin.com/ims/exchange/jwt",
                        body = list(
                            client_id = secrets$client_id,
                            client_secret = secrets$client_secret,
                            jwt_token = jwt_token
                        ),
                        encode = 'form')

    httr::stop_for_status(token)
    token
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
            "https://ims-na1.adobelogin.com/s/ent_analytics_bulk_ingest_sdk" = TRUE
        )

        jwt_token <- jose::jwt_encode_sig(jwt_claim, private_key, size = 256)
    }

    jwt_token
}


#' Adobe JWT token response
#'
#' Includes the response object containing the bearer token as well as the
#' credentials used to generate the token for seamless refreshing.
#'
#' Refreshing is disabled if the user used a custom JWT token.
#'
#' @section Methods:
#' * `refresh()`: refresh access token (if possible)
#' * `validate()`: TRUE if the token is still valid, FALSE otherwise
#'
#' @docType class
#' @keywords internal
#' @format An R6 object
#' @importFrom R6 R6Class
#' @export
AdobeJwtToken <- R6::R6Class("AdobeJwtToken", list(
    secrets = NULL,
    token = NULL,
    initialize = function(token, secrets) {
        self$secrets <- secrets
        self$token <- token
    },
    can_refresh = function() {
        !all(c(secrets$private_key, secrets$org_id, secrets$tech_id) == "")
    },
    refresh = function() {
        self$token <- auth_jwt_gen(self$secrets)
        self
    },
    validate = function() {
        self$token$date + httr::content(self$token)$expires_in / 1000 > Sys.time() - 1200
    }
))


# OAuth ----------------------------------------------------------------
#' @family auth
#' @describeIn aw_auth Authorize via OAuth 2.0
#' @export
auth_oauth <- function(client_id = Sys.getenv("AW_CLIENT_ID"),
                       client_secret = Sys.getenv("AW_CLIENT_SECRET"),
                       use_oob = TRUE) {
    stopifnot(is.character(client_id))
    stopifnot(is.character(client_secret))

    if (use_oob) {
        oob_value <- "https://adobeanalyticsr.com/token_result.html"
    } else {
        oob_value <- NULL
    }


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
        cache = token_path(getOption("adobeanalyticsr.auth_path", "aa.oauth")),
        use_oob = use_oob,
        oob_value = oob_value
    )

    message("Successfully authenticated with OAuth")
    .adobeanalytics$token <- token
}
