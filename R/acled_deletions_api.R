#' @title Request data from the ACLED deletions API
#' @name acled_deletions_api
#' @description This function allows users to pull deleted ACLED events from the API to keep their data up to date.
#' @param email character string. Email associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param key character string. Access key associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param date_deleted character string. Format 'yyyy-mm-dd' or Unix timestamp. The query will return all deleted events including and after the requested date/timestamp.
#' @param acled_access logical. If TRUE it means that you have utilized the acled_access function, thus there is no need for email and key arguments.
#' @returns Returns a tibble of of ACLED data with columns for event_id_cnty and deleted_timestamp.
#' @family API and Access
#' @seealso
#' \itemize{
#' \item \href{https://acleddata.com/download/35306/}{ACLED API guide}
#' \item \href{https://acleddata.com/download/35179/}{Keeping ACLED data up to date guide}
#' }
#' @examples
#' \dontrun{
#'
#' # Request deleted ACLED events since January 1 2022
#' acled_deletions_api(date_deleted = "2022-01-01", acled_acess = TRUE)
#'
#' }
#' @md
#' @import httr
#'
#' @export


acled_deletions_api <- function(email = NULL,
                                key = NULL,
                                date_deleted = NULL,
                                acled_access = TRUE) {

  if(acled_access == TRUE){
    email <- Sys.getenv("acled_email")
    key <- Sys.getenv("acled_key")
  }

  # Required components
  base_url <- "https://api.acleddata.com/deleted/read.csv?"

  if((!is.character(email) | is.na(email) | email == "") == TRUE) {
    stop("Email address required for ACLED API access. 'email' must be a character string (e.g., 'name@mail.com') or a call to where your email address is located if stored as an environment variable (e.g., Sys.getenv('email_adress'). Register your email for access at https://developer.acleddata.com.")
  }
  email_internal <- paste0("&email=", email)

  if((!is.character(key) | is.na(key) | key == "") == TRUE) {
    stop("Key required for ACLED API access. 'key' must be a character string (e.g., 'xyz123!etc') or a call to where your ACLED API key is located if stored as an environment variable (e.g., Sys.getenv('acled_key'). Request and locate your ACLED API key at https://developer.acleddata.com.")
  }
  key_internal <- paste0("&key=", key)



  # When
  if(!is.null(date_deleted)) {
    dates_internal <- paste0("&deleted_timestamp=", date_deleted)
  } else{dates_internal <- ""}





  url <- paste0(base_url,
                email_internal,
                key_internal,
                dates_internal,
                "&limit=0")



  response <- httr::GET(url)

  if(response[["status_code"]] == 500) {
    stop(paste0("API request unsuccessful with status code ", response[["status_code"]], ". \n",rlang::format_error_bullets(c("Make sure you have not execeeded your API calls (2/year for a standard account)","Verify your API credentials (key and email)", "If nothing works contact us through GitHub Issues or at access@acleddata.com."))))
  } else if(response[["status_code"]] == 503 | response[["status_code"]] == 502){
    stop(paste0("API request unsuccessful with status code ", response[["status_code"]], ". \n","Our server may be under maintenance or it may momentarily be unavailable; please try again in a couple of minutes."))
  }

  out <- content(response)

  return(out)

}


