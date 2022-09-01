acled_api2 <- function(email = NULL,
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



  if(!is.null(countries) & is.null(regions)) {
    df <- acledR::acled_countries %>%
      filter(country %in% countries)
  }
  else if(is.null(countries) & !is.null(regions)) {
    df <- acledR::acled_countries %>%
      filter(region %in% regions)
  }
  else if(!is.null(countries) & !is.null(regions)){
    df <- acledR::acled_countries %>%
      filter(country %in% countries & region %in% regions)
  }
  else {df <- acledR::acled_countries}

  if(is.null(start_date)) {start_date_check <- "1997-01-01"}
  if(is.null(end_date)) {end_date_check <- Sys.Date()}

  out <- df %>%
    mutate(t_start = lubridate::as_date(start_date_check),
           t_end = lubridate::as_date(end_date_check),

           t_start = case_when(as.numeric(lubridate::year(t_start)) < start_year ~ lubridate::as_date(paste0(start_year, "-01-01")),
                               TRUE ~ t_start),
           time = t_end - t_start)

  n_countries <- length(unique(out$country))
  country_days <- as.numeric(sum(out$time))

  size_note <- paste("Requesting data for",
                     n_countries,
                     "countries.",
                     "Accounting for the requested time period and ACLED coverage dates, this request includes",
                     format(country_days, big.mark = ","), "country-days.")


  time_units <- ceiling(country_days / 10000)

  out_groups <- split(out, sample(1:time_units, nrow(out), replace = T))

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



  url <- vector("list", length = length(out_groups))
  for(i in 1:length(out_groups)) {
    url[[i]] <- paste0(base_url, monadic_internal,
                       email_internal, key_internal,
                       countries_internal[[i]], regions_internal,
                       dates_internal, timestamp_internal,
                       event_types_internal, ..., "&limit=0")
  }

  response <- vector("list", length = length(out_groups))
  for(i in 1:length(out_groups)) {
    response[[i]] <- httr::GET(url[[i]])

    if(response[[i]][["status_code"]] == 500) {
      stop(paste0("API request unsuccessful with status code ", response[[i]][["status_code"]], ". \n",rlang::format_error_bullets(c("Make sure you have not execeeded your API calls (2/year for a standard account)","Verify your API credentials (key and email)", "If nothing works contact us through GitHub Issues or at access@acleddata.com."))))
    } else if(response[[i]][["status_code"]] == 503 | response[[i]][["status_code"]] == 502){
      stop(paste0("API request unsuccessful with status code ", response[[i]][["status_code"]], ". \n","Our server may be under maintenance or it may momentarily be unavailable; please try again in a couple of minutes."))
    }

  }

  out <- map_df(.x = response,
             ~content(.x))




  return(out)

}

temp <- acled_api2(acled_access = T,
           countries = c("Nigeria",
                         "Mexico",
                         "Niger",
                         "South Africa"))

