#' @title Request a token from ACLED API OAuth endpoint.
#' @name acled_token
#' @description Pings the ACLED API token endpoint for authentication using email and password.
#' @param req An ACLED API request.
#' @param email Email associated with ACLED account.
#' @param password User password.
#' @family API and Access
#' @returns ACLED API token
#' @examples
#' \dontrun{
#' acled_token(email, password)
#' }
#' @seealso ACLED API Access guide <https://acleddata.com/download/35300/>
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




acled_token <- function(email, password) {

  token_url <- "https://acleddata.com/oauth/token"

  token_data <- list(
    username = email,
    password = password,
    grant_type = "password",
    client_id = "acled"
  )

  response <- httr2::request(token_url) %>%
    httr2::req_body_multipart(!!!token_data) %>%
    httr2::req_perform()

  token_content <- httr2::resp_body_json(response)

  access_token <- token_content$access_token

  return(access_token)

}
