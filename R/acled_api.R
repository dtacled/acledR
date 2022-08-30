#' @title Request data from ACLED API
#' @name acled_api
#' @description This function allows users to easily request data in ACLED API. Users can include variables such as countries, regions, dates of interest and the type of file (monadic or dyadic). The function returns a tibble of the desired ACLED events.
#' @param email character string. Email associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param key character string. Access key associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param countries character vector. Default is NULL, which will return events for all countries. Pass a vector of country names to retrieve events from specific countries. The list of ACLED country names may be found via acledR::acled_countries,
#' @param regions vector of region names (character) or region codes (numeric). Default is NULL, which will return events for all regions.  Pass a vector of regions names or codes to retrieve events from countries within specific regions. The list of ACLED regions may be found via acledR::acled_regions,
#' @param event_types vector of one or more event types (character). Default is NULL, which will return data for all event types. To reurn data for only specific event types, request one or more of the following options (not case sensitive): Battles, Violence against civilians, Protests, Riots, Strategic Developments, and Explosions/Remote violence.
#' @param start_date character string. Format 'yyyy-mm-dd'. The earliest date for which to return events. The default is NULL, which will return events from all available time periods. If 'start_date' is NULL, 'end_date' must also be NULL.
#' @param end_date character string. Format 'yyyy-mm-dd'. The latest date for which to return events. The default is NULL, which will return events from all available time periods. If 'end_date' is NULL, 'start_date' must also be NULL.
#' @param monadic logical. If FALSE (default), returns dyadic data. If TRUE, returns monadic actor1 data.
#' @param timestamp numerical or character string. Provide a date or datetime written as either a character string of yyyy-mm-dd or as a numeric Unix timestamp to access all events added or updated after that date.
#' @param acled_access logical. If TRUE, you have used the acled_access function and the email and key arguments are not required.
#' @returns Returns a tibble of of ACLED events.
#' @family API and Access
#' @seealso
#' \itemize{
#' \item ACLED API guide. <https://acleddata.com/acleddatanew//wp-content/uploads/dlm_uploads/2021/11/API-User-Guide_Feb2022.pdf>
#' }
#' @examples
#' \dontrun{
#'
#' ## Get all the events coded by ACLED in Argentina from 01/01/2022 until 02/01/2022 in dyadic-wide form
#' argen_acled <- acled_api(jane.doe.email,jane.doe.key,countries = "Argentina",start_date = "2022-01-01",end_date="2022-02-01", acled_access = FALSE)
#'
#' ## tibble with all the events from Argentina where each row is one event.
#' argen_acled
#'
#' ## Get all events coded by ACLED in the Caribbean from 01/01/2022 to 10/01/2022 in monadic-long form
#' carib_acled <- acled_api(john.doe.email,john.doe.key,regions = "Caribbean",start_date = "2022-01-01",end_date="2022-01-10", monadic=TRUE, acled_access = FALSE)
#'
#' ## Tibble with all the events from the Caribbean where each row is one actor
#' carib_acled
#' }
#' @md
#' @import httr
#'
#' @export

acled_api <- function(email = NULL,
                      key = NULL,
                      countries = NULL,
                      regions = NULL,
                      start_date = NULL,
                      end_date = NULL,
                      timestamp = NULL,
                      event_types = NULL,
                      monadic = FALSE,
                      ...,
                      acled_access = TRUE) {

  if(acled_access == TRUE){
    email <- Sys.getenv("acled_email")
    key <- Sys.getenv("acled_key")
  }

  # Required components
  base_url <- "https://api.acleddata.com/acled/read.csv?"

  if((!is.character(email) | is.na(email) | email == "") == TRUE) {
    stop("Email address required for ACLED API access. 'email' must be a character string (e.g., 'name@mail.com') or a call to where your email address is located if stored as an environment variable (e.g., Sys.getenv('email_adress'). Register your email for access at https://developer.acleddata.com.")
  }
  email_internal <- paste0("&email=", email)

  if((!is.character(key) | is.na(key) | key == "") == TRUE) {
    stop("Key required for ACLED API access. 'key' must be a character string (e.g., 'xyz123!etc') or a call to where your ACLED API key is located if stored as an environment variable (e.g., Sys.getenv('acled_key'). Request and locate your ACLED API key at https://developer.acleddata.com.")
  }
  key_internal <- paste0("&key=", key)


  # Where
  ## Countries
  if(!is.null(countries) & sum(unique(countries) %in% acled_countries[["country"]]) < length(unique(countries))) {
    stop("One or more requested countries not in the ACLED country list. The full list of ACLED countries is available at 'acledR::acled_countries'.")
  }
  if(is.character(countries)) {
    countries_internal <- paste0("&country=", paste( gsub("\\s{1}", "%20", countries), collapse = ":OR:country="))
    countries_internal <- paste0(countries_internal, "&country_where=%3D")
  }
  if(is.null(countries)) {
    countries_internal <- ""
  }

  ## Regions
  if(is.character(regions) & sum(unique(regions) %in% acled_regions[["region_name"]]) < length(unique(regions))) {
    stop("One or more requested region names not in the ACLED country list. The full list of ACLED regions is available at 'acledR::acled_regions'.")
  }
  if(is.numeric(regions) & sum(unique(regions) %in% acled_regions[["region"]]) < length(unique(regions))) {
    stop("One or more requested region numbers not in the ACLED country list. The full list of ACLED regions is available at 'acledR::acled_regions'.")
  }
  if(is.numeric(regions)) {
    regions_internal <- paste0("&region=", paste(gsub("\\s{1}", "%20", regions), collapse = ":OR:region="))
  }
  if(is.character(regions)) {
    regions <- filter(acled_regions, region_name %in% regions) %>%
      pull(region)
    regions_internal <- paste0("&region=", paste(gsub("\\s{1}", "%20", regions), collapse = ":OR:region="))
  }
  if(is.null(regions)) {
    regions_internal <- ""
  }



  # Dates
  if(!is.null(start_date) & !is.null(end_date)) {
    dates_internal <- paste0("&event_date=", paste(start_date, end_date, sep = "|"), "&event_date_where=BETWEEN")
  }
  if(is.null(start_date) != is.null(end_date)) {
    stop("Both 'start_date' and 'end_date' must be specified if a specific time period is requested. To request all time periods, leave both 'start_date' and 'end_date' NULL.")
  }
  if(!is.null(start_date) & !is.null(end_date)){
    if(start_date > end_date) {
      stop("Requested 'start_date' is after the requested 'end_date'.")
    }
  }
  if(is.null(start_date) & is.null(end_date)) {
    dates_internal <- ""
  }


  # Timestamps
  if(!is.null(timestamp)) {

    timestamp_into_date <- tryCatch({

      lubridate::ymd(timestamp)

      timestamp_into_date <- "string"
    },
    warning = function(w){
      a <- "numerical"
    },
    error = function(e){
      a <- "numerical"
    })

    if(timestamp_into_date == "string"){
      timestamp_parsable <- lubridate::ymd(timestamp)
      do_i_include_timestamp <- "Yes"
    } else {
      timestamp_parsable <- tryCatch({
        lubridate::date(lubridate::as_datetime(timestamp))
        do_i_include_timestamp <- "Yes_but_numerical"

      },
      warning = function(w){
        za <- utils::menu(c("Yes","No"),
                          title=paste0("You indicated a timestamp, but it was not recognized. Reminder: Timestamp only accepts string as yyyy-mm-dd OR a Unix timestamp (integer).", "\n", "\n","Do you want me to continue and ignore timestamp?"))
        if(za == 1){
          assign("do_i_include_timestamp","No")} else{
            stop("User requested to abort when timestamp was not recognized.")
          }
      },
      error = function(e){
        stop("User requested to abort when timestamp was not recognized.")})

    }


    if(do_i_include_timestamp == "Yes"){
      if(timestamp_parsable > lubridate::now()) {
        stop("The timestamp cannot be later than today. Please change the timestamp and try again.")
      }
      else if(is.na(timestamp_parsable)){
        stop("The timestamp must be formated as YYYY-MM-DD")
      }
      else{
        timestamp_internal <- paste0("&timestamp=", timestamp_parsable)}
    }
    else if(do_i_include_timestamp == "Yes_but_numerical"){
      timestamp_internal <- paste0("&timestamp=", timestamp)
    }
    else{
      timestamp_internal <- "&timestamp="}}
  else{
    timestamp_internal <- "&timestamp="}

  # How
  if(isTRUE(monadic))
    monadic_internal <- "&export_type=monadic"
  else
    monadic_internal <- ""



  # Event types
  if(!is.null(event_types)) {
    event_types <- str_to_upper(event_types)
    if(FALSE %in% unique(event_types %in% str_to_upper(c("Battles", "Violence against civilians", "Protests",
                                       "Riots", "Strategic Developments", "Explosions/Remote violence")))) {
      stop("One or more requested event types are not in the ACLED data. Event types include: Battles, Violence against civilians, Protests, Riots, Strategic Developments, and Explosions/Remote violence. Leave 'event_type = NULL' to request all event types from the API. ")
    }

    event_types_internal <- paste0("&event_type=", paste(gsub("\\s{1}", "%20", event_types), collapse = ":OR:event_type="))
  }
  else
    event_types_internal <- ""




  url <- paste0(base_url, monadic_internal,
                email_internal, key_internal,
                countries_internal, regions_internal,
                dates_internal, timestamp_internal,
                event_types_internal, ..., "&limit=0")

  try

  response <- httr::GET(url)

  if(response[["status_code"]] == 500) {
    stop(paste0("API request unsuccessful with status code ", response[["status_code"]], ". \n",rlang::format_error_bullets(c("Make sure you have not execeeded your API calls (2/year for a standard account)","Verify your API credentials (key and email)", "If nothing works contact us through GitHub Issues or at access@acleddata.com."))))
  } else if(response[["status_code"]] == 503 | response[["status_code"]] == 502){
    stop(paste0("API request unsuccessful with status code ", response[["status_code"]], ". \n","Our server may be under maintenance or it may momentarily be unavailable; please try again in a couple of minutes."))
  }

  out <- content(response)

  return(out)

}
