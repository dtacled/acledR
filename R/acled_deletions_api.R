#' @title Request data from the ACLED Deletions API
#' @name acled_deletions_api
#' @description This function allows users to pull deleted ACLED event IDs from the Deletions API.
#' @param email character string. Email associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param password character string. The password associated with your ACLED account. If NULL, you will be prompted to enter your password interactively.
#' @param date_deleted character string. Format 'yyyy-mm-dd' or Unix timestamp. The query will return all deleted events including and after the requested date/timestamp.
#' @returns Returns a tibble of ACLED data with columns for event_id_cnty and deleted_timestamp.
#' @family API and Access
#' @seealso
#' \itemize{
#' \item \href{https://acleddata.com/api-documentation/deleted-endpoint}{ACLED API deleted endpoint}
#' }
#' @examples
#' \dontrun{
#'
#' # Request deleted ACLED events since January 1, 2022
#' acled_deletions_api(date_deleted = "2022-01-01")
#' }
#' @md
#' @import httr
#'
#' @export


acled_deletions_api <- function(email = NULL,
                                password = NULL,
                                date_deleted = NULL) {



  if (is.null(email)) {
    stop("Email not specified. You must include the email associated with your ACLED account in the `email` parameter.")
  }

  if (is.null(password)) {
    stop("Password not specified. You must include the email associated with your ACLED account in the `password` parameter.")
  }


  base_url <- "https://acleddata.com/api/deleted/read?_format=json"


  # When

  # Check if timestamp or date; if date convert to timestamp
  if(str_detect(date_deleted, "-")) {
    date_deleted <- as.numeric(as.POSIXct("2022-07-25", format="%Y-%m-%d"))
  }


  if (!is.null(date_deleted)) {
    dates_internal <- paste0("&deleted_timestamp_where=%3E%3D&deleted_timestamp=", date_deleted)
  } else {
    dates_internal <- ""
  }

  url <- paste0(
    base_url,
    dates_internal,
    "&limit=0"
  )


  response <- httr2::request(url) %>%
    acled_auth(username = email, password = password) %>%
    httr2::req_perform()


  if (response[["status_code"]] == 500) {
    stop(paste0("API request unsuccessful with status code ", response[["status_code"]], ". \n", rlang::format_error_bullets(c("Make sure you have not execeeded your API calls (2/year for a standard account)", "Verify your API credentials (key and email)", "If nothing works contact us through GitHub Issues or at access@acleddata.com."))))
  } else if (response[["status_code"]] == 503 | response[["status_code"]] == 502) {
    stop(paste0("API request unsuccessful with status code ", response[["status_code"]], ". \n", "Our server may be under maintenance or it may momentarily be unavailable; please try again in a couple of minutes."))
  }

  out <- suppressMessages(
    httr2::resp_body_json(response, simplifyVector = T)$data %>% as_tibble()
  )


  return(out)
}
