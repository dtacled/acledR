#' @title Authenticate a request via ACLED API OAuth endpoint.
#' @name acled_auth
#' @description Pings the ACLED API token endpoint for authentication using email and password.
#' @param req An ACLED API request.
#' @param username Email associated with ACLED account.
#' @param password User password associated with ACLED account.
#' @family API and Access
#' @returns Returns a modified HTTP request that will use OAuth for the ACLED API.
#' @examples
#' \dontrun{
#' acled_auth(req, email, password)
#' }
#' @seealso ACLED API Access guide <https://acleddata.com/api-documentation/getting-started>
#' @export
#' @md


acled_auth <- function(req, username, password = NULL) {

  token_url <- "https://acleddata.com/oauth/token"

  httr2::req_oauth_password(req,
                     client = httr2::oauth_client("acled", token_url),
                     username = username,
                     password = password
  )

}
