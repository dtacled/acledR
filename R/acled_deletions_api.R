#' @title Request data from the ACLED Deletions API
#' @name acled_deletions_api
#' @description This function allows users to pull deleted ACLED event IDs from the Deletions API.
#' @param email character string. Email associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param password character string. The password associated with your ACLED account. If NULL, you will be prompted to enter your password interactively.
#' @param key character string. Access key associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param date_deleted character string. Format 'yyyy-mm-dd' or Unix timestamp. The query will return all deleted events including and after the requested date/timestamp.
#' @param acled_access logical. If TRUE it means that you have utilized the acled_access function and there is no need for the email and key arguments.
#' @param log Only for testing purposes: you can use this to check if all the variables in your call were handled properly.
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
#' acled_deletions_api(date_deleted = "2022-01-01", acled_acess = TRUE)
#' }
#' @md
#' @import httr
#'
#' @export


acled_deletions_api <- function(email = NULL,
                                key = NULL,
                                password = NULL,
                                date_deleted = NULL,
                                acled_access = FALSE,
                                log = FALSE) {


  # if ((acled_access %in% c(TRUE, T)) & (is.null(email) | is.null(key))) { # Access is true, and credentials are null
  #   email <- Sys.getenv("acled_email")
  #   key <- Sys.getenv("acled_key")
  #   if (nchar(email) <= 1 | nchar(key) <= 1) {
  #     stop("Error in credentials: \n  acled_access is TRUE, but email and/or key are not stored in the enviornment. Please rerun acled_access or include key and email in function")
  #   }
  # } else if ((acled_access %in% c(TRUE, T)) & (!is.null(email) | !is.null(key))) {
  #   message("acled_access is TRUE, but email and key are included in the function. Ignoring acled_access.")
  # }
  #
  # # Required components
  # base_url <- "https://api.acleddata.com/deleted/read.csv?"
  #
  # if ((!is.character(email) || is.null(email) || email == "") == TRUE) {
  #   stop("Email address required for ACLED API access. 'email' must be a character string (e.g., 'name@mail.com') or a call to where your email address is located if stored as an environment variable (e.g., Sys.getenv('email_adress'). Register your email for access at https://developer.acleddata.com.")
  # }
  # email_internal <- paste0("&email=", email)
  #
  # if ((!is.character(key) || is.null(key) || key == "") == TRUE) {
  #   stop("Key required for ACLED API access. 'key' must be a character string (e.g., 'xyz123!etc') or a call to where your ACLED API key is located if stored as an environment variable (e.g., Sys.getenv('acled_key'). Request and locate your ACLED API key at https://developer.acleddata.com.")
  # }
  # key_internal <- paste0("&key=", key)

  if (!is.null(key)) {
    lifecycle::deprecate_warn(when = "1.0.0",
                              what =  "acled_api(key)",
                              details = c("The ACLED API will no longer support access keys as of September 2025.",
                                          "Please use the `password` parameter instead of `key`.",
                                          "The `key` parameter will be removed after it's no longer functional in the ACLED API."),
                              always = TRUE)
  }

  if (isTRUE(acled_access)) {
    lifecycle::deprecate_warn(when = "1.0.0",
                              what =  "acled_api(acled_access)",
                              details = c("The ACLED API has transitioned to OAuth authentication.",
                                          "The `acled_access` function will is no longer necessary and will stop working in September 2025.",
                                          "Please use the `email` and `password` parameters instead.",
                                          "`acled_access` will be removed in future iterations of acledR."),
                              always = TRUE)
  }

  # Check if going the old or new route
  if ((acled_access %in% c(TRUE, T)) | !is.null(key)) {
    route <- 'key'
    # message("Using the email and key API authentication option.\nThis method is deprecated in favor of OAuth and will expire soon.\nSee xyz for details on this transition.")
  }
  else {
    route <- "oauth"
  }


  if (route == "key") {
    if ((acled_access %in% c(TRUE, T)) & (is.null(email) | is.null(key))) { # Access is true, and credentials are null
      email <- Sys.getenv("acled_email")
      key <- Sys.getenv("acled_key")
      if (nchar(email) <= 1 | nchar(key) <= 1) {
        stop("Error in credentials: \n  acled_access is TRUE, but email and/or key are not stored in the enviornment. Please rerun acled_access or include key and email in function")
      }
    } else if ((acled_access %in% c(TRUE, T)) & (!is.null(email) | !is.null(key))) {
      message("acled_access is TRUE, but email and key are included in the function. Ignoring acled_access.")
    }
  }

  if (route == "oauth") {
    if (is.null(email)) {
      stop("Error in credentials: \n if trying to use the OAuth route, you must specify your email and password. \n Note that the key and acled_access parameters are not used if using OAuth, only email and password.")
    }
  }



  if (route == "key") {
    if (!is.character(email) || is.null(email) || (is.character(email) && nchar(email) < 3)) {
      stop("Email address required for ACLED API access. 'email' must be a character string (e.g., 'name@mail.com') or a call to where your email address is located if stored as an environment variable (e.g., Sys.getenv('acled_email'). Register your email for access at https://developer.acleddata.com.")
    }
    email_internal <- paste0("&email=", email)

    if ((!is.character(key) || is.null(key) || key == "") == TRUE) {
      stop("Key required for ACLED API access. 'key' must be a character string (e.g., 'xyz123!etc') or a call to where your ACLED API key is located if stored as an environment variable (e.g., Sys.getenv('acled_key'). Request and locate your ACLED API key at https://developer.acleddata.com.")
    }
    key_internal <- paste0("&key=", key)
  }


  # Required components
  if (route == "key") {
    base_url <- "https://api.acleddata.com/deleted/read.csv?"
  }
  if (route == "oauth") {
    base_url <- "https://acleddata.com/api/deleted/read?_format=json"
  }


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

  if (route == "key") {
    url <- paste0(
      base_url,
      email_internal,
      key_internal,
      dates_internal,
      "&limit=0"
    )
  }

  if (route == "oauth") {
    url <- paste0(
      base_url,
      dates_internal,
      "&limit=0"
    )
  }


  if (log == T & route == "key") {
    log_df <- tibble(email = email, key = key, date_deleted = date_deleted)
    return(log_df)
  }

  if (log == T & route == "oauth") {
    log_df <- tibble(email = email, date_deleted = date_deleted)
    return(log_df)
  }

  if (route == "key") {
    response <- httr::GET(url)
  }

  if (route == "oauth") {
    response <- httr2::request(url) %>%
      acled_auth(username = email, password = password) %>%
      httr2::req_perform()

  }



  if (response[["status_code"]] == 500) {
    stop(paste0("API request unsuccessful with status code ", response[["status_code"]], ". \n", rlang::format_error_bullets(c("Make sure you have not execeeded your API calls (2/year for a standard account)", "Verify your API credentials (key and email)", "If nothing works contact us through GitHub Issues or at access@acleddata.com."))))
  } else if (response[["status_code"]] == 503 | response[["status_code"]] == 502) {
    stop(paste0("API request unsuccessful with status code ", response[["status_code"]], ". \n", "Our server may be under maintenance or it may momentarily be unavailable; please try again in a couple of minutes."))
  }


  if (route == "key") {
    out <- suppressMessages(
      content(response)
    )
  }

  if (route == "oauth") {
    out <- suppressMessages(
      httr2::resp_body_json(response, simplifyVector = T)$data %>% as_tibble()
    )
  }


  return(out)
}
