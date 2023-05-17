#' @title Request data from ACLED API
#' @name acled_api
#' @description This function allows users to easily request data from the ACLED API. Users can include variables such as countries, regions, dates of interest and the type of file (monadic or dyadic). The function returns a tibble of the desired ACLED events.
#' @param email character string. Email associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param key character string. Access key associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param countries character vector. Default is NULL, which will return events for all countries. Pass a vector of country names to retrieve events from specific countries. The list of ACLED country names may be found via acledR::acled_countries.
#' @param regions vector of region names (character) or region codes (numeric). Default is NULL, which will return events for all regions.  Pass a vector of regions names or codes to retrieve events from countries within specific regions. The list of ACLED regions may be found via acledR::acled_regions.
#' @param start_date character string. Format 'yyyy-mm-dd'. The earliest date for which to return events. The default is `1997-01-01`, which is the earliest date available.
#' @param end_date character string. Format 'yyyy-mm-dd'. The latest date for which to return events. The default is Sys.Date(), which is the most present date.
#' @param timestamp numerical or character string. Provide a date or datetime written as either a character string of yyyy-mm-dd or as a numeric Unix timestamp to access all events added or updated after that date.
#' @param event_types vector of one or more event types (character). Default is NULL, which will return data for all event types. To reurn data for only specific event types, request one or more of the following options (not case sensitive): Battles, Violence against civilians, Protests, Riots, Strategic Developments, and Explosions/Remote violence.
#' @param monadic logical. If FALSE (default), returns dyadic data. If TRUE, returns monadic actor1 data.
#' @param ... string. Any additional parameters that users would like to add to their API calls (e.g. interaction or ISO)
#' @param acled_access logical. If TRUE (default), you have used the acled_access function and the email and key arguments are not required.
#' @param log logical. If TRUE, it provides a dataframe with the countries and days requested, and how many calls it entails. The dataframe is provided INSTEAD of the normal ACLED dataset.
#' @param prompt logical. If TRUE (default), users will receive an interactive prompt providing information about their call (countries requested, number of country-days, and number of API calls required) and asking if they want to proceed with the call. If FALSE, the call continues without warning, but the call is split and returns a message specifying how many calls are being made.
#' @returns Returns a tibble of of ACLED events.
#' @family API and Access
#' @seealso
#' \itemize{
#' \item ACLED API guide. <https://acleddata.com/acleddatanew//wp-content/uploads/dlm_uploads/2021/11/API-User-Guide_Feb2022.pdf>
#' }
#' @examples
#' \dontrun{
#'
#' # Get all the events coded by ACLED in Argentina from 01/01/2022 until 02/01/2022
#' # in dyadic-wide form
#' argen_acled <- acled_api(email = jane.doe.email, key = jane.doe.key,
#'                         countries = "Argentina", start_date = "2022-01-01", end_date="2022-02-01",
#'                         acled_access = FALSE)
#'
#' # tibble with all the events from Argentina where each row is one event.
#' argen_acled
#'
#' # Get all events coded by ACLED in the Caribbean from 01/01/2022 to 10/01/2022
#' # in monadic-long form using email and key saved in environment
#'
#' acled_access(email = "jane.doe.email", key = "jane.doe.key")
#' carib_acled <- acled_api(regions = "Caribbean", start_date = "2022-01-01",
#'                          end_date="2022-01-10", monadic=TRUE, acled_access = TRUE)
#'
#' ## Tibble with all the events from the Caribbean where each row is one actor
#' carib_acled
#' }
#' @md
#' @import httr
#' @import dplyr
#' @import stringr
#' @import purrr
#' @import lubridate
#' @importFrom rlang .data
#' @importFrom methods hasArg
#' @export

acled_api <- function(email = NULL,
                       key = NULL,
                       countries = NULL,
                       regions = NULL,
                       start_date = floor_date(Sys.Date(), "year") - year(1),
                       end_date = Sys.Date(),
                       timestamp = NULL,
                       event_types = NULL,
                       monadic = FALSE,
                       ...,
                       acled_access = TRUE,
                       prompt = TRUE,
                       log = F) {

  # Acled Acess and credentials

  if((acled_access %in% c(TRUE,T))&(is.null(email)|is.null(key))){  # Access is true, and credentials are null
    email <- Sys.getenv("acled_email")
    key <- Sys.getenv("acled_key")
    if(nchar(email) <= 1 | nchar(key) <= 1){
      stop("Error in credentials: \n  acled_access is TRUE, but email and/or key are not stored in the enviornment. Please rerun acled_access or include key and email in function")
    }

  } else if ((acled_access %in% c(TRUE,T))&(!is.null(email)|!is.null(key))){
    message("acled_access is TRUE, but email and key are included in the function. Ignoring acled_access.")
  }


  # Stoppers for typos

  if(hasArg("country") | hasArg("Country")){
    stop("Country is not a valid option. Please utilize \"countries\"")

  }

  if(hasArg(Countries)){
    stop("Countries is not a valid option. Please utilize \"countries\", without capitalizing")

  }

  if(hasArg(region)|hasArg(Region)){
    stop("Region is not a valid option. Please utilize \"regions\"")

  }

  if(hasArg(Regions)){
    stop("Regions is not a valid option. Please utilize \"regions\", without capitalizing")

  }

  if(hasArg(event_type)){
    stop("event type is not a valid option. Please utilize \"event_types\"")

  }

  if(hasArg(Event_type)){
    stop("Event type is not a valid option. Please utilize \"event_types\", without capitalizing")

  }

  if(hasArg(Start_date)){
    stop("Start_date is not a valid option. Please utilize \"start_date\", without capitalizing")

  }

  if(hasArg(End_date)){
    stop("End_date is not a valid option. Please utilize \"end_date\", without capitalizing")

  }

  # Required components
  base_url <- "https://api.acleddata.com/acled/read.csv?"

  if((!is.character(email) | is.na(email) | email == "") == TRUE) {
    stop("Email address required for ACLED API access. 'email' must be a character string (e.g., 'name@mail.com') or a call to where your email address is located if stored as an environment variable (e.g., Sys.getenv('acled_email'). Register your email for access at https://developer.acleddata.com.")
  }
  email_internal <- paste0("&email=", email)

  if((!is.character(key) | is.na(key) | key == "") == TRUE) {
    stop("Key required for ACLED API access. 'key' must be a character string (e.g., 'xyz123!etc') or a call to where your ACLED API key is located if stored as an environment variable (e.g., Sys.getenv('acled_key'). Request and locate your ACLED API key at https://developer.acleddata.com.")
  }
  key_internal <- paste0("&key=", key)

  # Checking if countries are input incorrectly
  if(!is.null(countries) & !is.character(countries)){
    stop("Countries are not strings, please state them as string/character")
  }

  if(!is.null(countries) & sum(unique(countries) %in% acledR::acled_countries[["country"]]) < length(unique(countries))) {
    stop("One or more of the requested countries are not in ACLED's Country list. The full list of countries is available at 'acledR::acled_countries")
  }

  # Checking if regions are input incorrectly
  if(is.character(regions) & sum(unique(regions) %in% acledR::acled_regions[["region_name"]]) < length(unique(regions))) {
    stop("One or more requested region names not in the ACLED country list. The full list of ACLED regions is available at 'acledR::acled_regions'.")
  }
  if(is.numeric(regions) & sum(unique(regions) %in% acledR::acled_regions[["region"]]) < length(unique(regions))) {
    stop("One or more requested region numbers not in the ACLED country list. The full list of ACLED regions is available at 'acledR::acled_regions'.")
  }





  # Setup base data to check how many country-days are being requested
  if(!is.null(countries) & is.null(regions)) {
    df <- acledR::acled_countries %>%
      filter(.data$country %in% countries)
  }
  else if(is.null(countries) & !is.null(regions)) {
    if(is.numeric(regions)){
      regions <- acledR::acled_regions %>%
        filter(.data$region %in% regions) %>%
        pull(.data$region_name)}

    df <- acledR::acled_countries %>%
      filter(.data$region %in% regions)
  }
  else if(!is.null(countries) & !is.null(regions)){

    if(is.numeric(regions)){
      regions <- acledR::acled_regions %>%
        filter(.data$region %in% regions) %>%
        pull(.data$region_name)}

    df <- acledR::acled_countries %>%
      filter(.data$country %in% countries & .data$region %in% regions)
  }
  else {df <- acledR::acled_countries}

  if(is.null(start_date)) {
    start_date_check <- "1997-01-01"
  }
  else {start_date_check <- start_date}

  if(is.null(end_date)) {
    end_date_check <- Sys.Date()
  }
  else {end_date_check <- end_date}

  out <- df %>%
    mutate(t_start = lubridate::as_date(start_date_check),
           t_end = lubridate::as_date(end_date_check),
           t_start = case_when(as.numeric(lubridate::year(t_start)) < start_year ~ lubridate::as_date(paste0(start_year, "-01-01")),
                               TRUE ~ t_start),
           time = .data$t_end - .data$t_start)

  n_countries <- length(unique(out$country))
  country_days <- as.numeric(sum(out$time))



  # Note for how much data is being requested
  size_note <- paste("Requesting data for",
                     n_countries,
                     "countries.",
                     "Accounting for the requested time period and ACLED coverage dates, this request includes",
                     format(country_days, big.mark = ","), "country-days.")

  message(size_note)


  # Approx how many calls are required with 1 call sized at 300k country-days
  time_units <- ceiling(country_days / 300000)

  # Split call into roughly equally sized groups depending on how many country-days are in each country
  # This randomly assigns countries into bins
  out_groups <- split(out, sample(1:time_units, nrow(out), replace = T))

  if(log == T){


    if (length(out_groups) > 1) {
      log_rep <- map_dfr(out_groups, bind_rows, .id= "id")%>%
        mutate(calls = time_units)
    } else {
      log_rep <- out_groups[[1]]
      log_rep$id <- "1"
      log_rep$calls <- time_units
    }

    log_rep$email <- email
    log_rep$key <- key

    return(log_rep)

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

  # Where
  ## Countries

  countries_internal <- vector("list", length = length(out_groups))
  for(i in 1:length(out_groups)){
    countries_internal[[i]] <- paste0("&country=", paste( gsub("\\s{1}", "%20", out_groups[[i]]$country), collapse = ":OR:country="))
    countries_internal[[i]] <- paste0(countries_internal[[i]], "&country_where=%3D")
  }

  ## Regions
  if(is.numeric(regions)) {
    regions_internal <- paste0("&region=", paste(gsub("\\s{1}", "%20", regions), collapse = ":OR:region="))
  }
  if(is.character(regions)) {
    regions <- acledR::acled_regions %>%
      filter(.data$region_name %in% regions) %>%
      pull(.data$region)
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




  # Interactive choice for users after prompting how many calls are required

  if(prompt == TRUE){

    message(paste0("This request requires ",
                   time_units,
                   " API calls. Do you want to proceed with this request?\nIf you need to increase your API quota, please contact access@acleddata.com"))

    if(interactive()) {
      user_input <- readline("Proceed? (Yes/No) \n")
      if(!((user_input == "Yes") | (user_input == "yes"))){
        stop('User responded "No" when prompted about the number of API calls required. \nIf you need to increase your API quota, please contact access@acleddata.com',
             call. = F)
        }
    }} else { message("Proceeding with ",
                   time_units,
                   " API calls")}

  # Loop through country bins to define each api call
  url_internal <- vector("list", length = length(out_groups))
  for(i in 1:length(out_groups)) {
    url_internal[[i]] <- paste0(base_url, monadic_internal,
                                email_internal, key_internal,
                                countries_internal[[i]], regions_internal,
                                dates_internal, timestamp_internal,
                                event_types_internal, ..., "&limit=0")
  }

  # Loop through the api requests
  response <- vector("list", length = length(out_groups))
  message("Processing API request")
  for(i in 1:length(out_groups)) {

    response[[i]] <- httr::GET(url_internal[[i]])

    if(response[[i]][["status_code"]] == 500) {
      stop(paste0("API request unsuccessful with status code ", response[[i]][["status_code"]], ". \n",rlang::format_error_bullets(c("Make sure you have not execeeded your API calls (2/year for a standard account)","Verify your API credentials (key and email)", "If nothing works contact us through GitHub Issues or at access@acleddata.com."))))
    } else if(response[[i]][["status_code"]] == 503 | response[[i]][["status_code"]] == 502){
      stop(paste0("API request unsuccessful with status code ", response[[i]][["status_code"]], ". \n","Our server may be under maintenance or it may momentarily be unavailable; please try again in a couple of minutes."))
    }
  }

  # Map through each get request to convert to one tibble
  message("Extracting content from API request")
  out <- suppressMessages(purrr::map_df(.x = response,
                                 ~content(.x)))


  return(out)


}
