#' @title Request data from ACLED API
#' @name acled_api
#' @description Processes requests to ACLED's API
#' @param email character string. Email associated with your ACLED account registered at https://developer.acleddata.com.
#' @param key character string. Access key associated with your ACLED account registered at https://developer.acleddata.com.
#' @param countries character vector. Default is NULL, which will return events for all countries. Pass a vector of country names to retrieve events from specific countries. The list of ACLED country names may be found via acledR::acled_countries,
#' @param regions vector of region names (character) or region codes (numeric). Default is NULL, which will return events for all regions.  Pass a vector of regions names or codes to retrieve events from countries within specific regions. The list of ACLED regions may be found via acledR::acled_regions,
#' @param start_date character string. Format 'yyyy-mm-dd'. The earliest date for which to return events. The default is NULL, which will return events from all available time periods. If 'start_date' is NULL, 'end_date' must also be NULL.
#' @param end_date character string. Format 'yyyy-mm-dd'. The latest date for which to return events. The default is NULL, which will return events from all available time periods. If 'end_date' is NULL, 'start_date' must also be NULL.
#' @param monadic logical. If FALSE (default), returns dyadic data. If TRUE, returns monadic data.
#' @returns Returns a tibble of of ACLED events.
#' @import httr
#'
#' @export

acled_api <- function(email, key, countries = NULL, regions = NULL,
                      start_date = NULL, end_date = NULL, monadic = FALSE) {

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


  # When
  if(!is.null(start_date) & !is.null(end_date)) {
    dates_internal <- paste0("&event_date=", paste(start_date, end_date, sep = "|"), "&event_date_where=BETWEEN")
  }
  if(is.null(start_date) != is.null(end_date)) {
    stop("Both 'start_date' and 'end_date' must be specified if a specific time period is requested. To request all time periods, leave both 'start_date' and 'end_date' NULL.")
  }
  if(start_date > end_date) {
    stop("Requested 'start_date' is after the requested 'end_date'.")
  }
  if(is.null(start_date) & is.null(end_date)) {
    dates_internal <- ""
  }

  # What
  ## TO DO (event types, inter codes)

  if(isTRUE(monadic))
    monadic_internal <- "&export_type=monadic"
  else
    monadic_internal <- ""


  url <- paste0(base_url, monadic_internal,
                email_internal, key_internal,
                countries_internal, regions_internal,
                dates_internal,
                "&limit=0")

  response <- GET(url)
  if(response[["status_code"]] != 200) {
    stop(paste0("API request unsuccessful with status code ", response[["status_code"]], "."))
    }

  out <- content(response)

  return(out)

  }


