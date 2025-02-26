#' @title Request data from ACLED API
#' @name acled_api
#' @description This function allows users to easily request data from the ACLED API. Users can include variables such as country, regions, dates of interest and the format (monadic or dyadic). The function returns a tibble of the desired ACLED events.
#' @param email character string. Email associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param key character string. Access key associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param country character vector. Default is NULL, which will return events for all countries. Pass a vector of countries names to retrieve events from specific countries. The list of ACLED countries. names may be found via acledR::acled_countries.
#' @param regions vector of region names (character) or region codes (numeric). Default is NULL, which will return events for all regions.  Pass a vector of regions names or codes to retrieve events from countries. within specific regions. The list of ACLED regions may be found via acledR::acled_regions.
#' @param start_date character string. Format 'yyyy-mm-dd'. The earliest date for which to return events. The default is `1997-01-01`, which is the earliest date available.
#' @param end_date character string. Format 'yyyy-mm-dd'. The latest date for which to return events. The default is Sys.Date(), which is the most present date.
#' @param timestamp numerical or character string. Provide a date or datetime written as either a character string of yyyy-mm-dd or as a numeric Unix timestamp to access all events added or updated after that date.
#' @param event_types vector of one or more event types (character). Default is NULL, which will return data for all event types. To reurn data for only specific event types, request one or more of the following options (not case sensitive): Battles, Violence against civilians, Protests, Riots, Strategic Developments, and Explosions/Remote violence.
#' @param population character. Specify whether to return population estimates for each event. It accepts three options: "none" (default), "best", and "full".
#' @param inter_numeric logical. If FALSE (default), interaction code columns (inter1, inter2, and interaction) returned as strings describing the actor types/interactions. If TRUE, the values are returned as numeric values.
#' @param monadic logical. If FALSE (default), returns dyadic data. If TRUE, returns monadic actor1 data.
#' @param ... string. Any additional parameters that users would like to add to their API calls (e.g. interaction or ISO)
#' @param acled_access logical. If TRUE (default), you have used the acled_access function and the email and key arguments are not required.
#' @param log logical. If TRUE, it provides a dataframe with the countries and days requested, and how many calls it entails. The dataframe is provided INSTEAD of the normal ACLED dataset.
#' @param prompt logical. If TRUE (default), users will receive an interactive prompt providing information about their call (countries requested, number of estimated events, and number of API calls required) and asking if they want to proceed with the call. If FALSE, the call continues without warning, but the call is split and returns a message specifying how many calls are being made.
#' @returns Returns a tibble of of ACLED events.
#' @family API and Access
#' @seealso
#' \itemize{
#' \item ACLED API guide. <https://apidocs.acleddata.com/>
#' }
#' @examples
#' \dontrun{
#'
#' # Get all the events coded by ACLED in Argentina from 01/01/2022 until 02/01/2022
#' # in dyadic-wide form
#' argen_acled <- acled_api(
#'   email = "your_email", key = "your_key",
#'   country = "Argentina", start_date = "2022-01-01", end_date = "2022-02-01",
#'   acled_access = FALSE
#' )
#'
#' # tibble with all the events from Argentina where each row is one event.
#' argen_acled
#'
#' # Get all events coded by ACLED in the Caribbean from 01/01/2022 to 10/01/2022
#' # in monadic-long form using email and key saved in environment
#'
#' acled_access(email = "your_email", key = "your_key")
#' carib_acled <- acled_api(
#'   regions = "Caribbean", start_date = "2022-01-01",
#'   end_date = "2022-01-10", monadic = TRUE, acled_access = TRUE
#' )
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
#' @importFrom utils menu
#' @importFrom methods hasArg
#' @export

acled_api <- function(email = NULL,
                       key = NULL,
                       country = NULL,
                       regions = NULL,
                       start_date = floor_date(Sys.Date(), "year") - years(1),
                       end_date = Sys.Date(),
                       timestamp = NULL,
                       event_types = NULL,
                       population = "none",
                      inter_numeric = FALSE,
                      monadic = FALSE,
                      ...,
                      acled_access = TRUE,
                       prompt = TRUE,
                       log = FALSE) {

  # Acled Acess and credentials ----

  if ((acled_access %in% c(TRUE, T)) & (is.null(email) | is.null(key))) { # Access is true, and credentials are null
    email <- Sys.getenv("acled_email")
    key <- Sys.getenv("acled_key")
    if (nchar(email) <= 1 | nchar(key) <= 1) {
      stop("Error in credentials: \n  acled_access is TRUE, but email and/or key are not stored in the enviornment. Please rerun acled_access or include key and email in function")
    }
  } else if ((acled_access %in% c(TRUE, T)) & (!is.null(email) | !is.null(key))) {
    message("acled_access is TRUE, but email and key are included in the function. Ignoring acled_access.")
  }


  # Stoppers for typos ----

  if (hasArg("Country")) {
    stop("Country is not a valid option. Please utilize \"country\", without capitalizing ")
  }

  if (hasArg("Region")) {
    stop("Region is not a valid option. Please utilize \"regions\"")
  }

  if (hasArg("Regions")) {
    stop("Regions is not a valid option. Please utilize \"regions\", without capitalizing")
  }

  if (hasArg("Event_type")) {
    stop("Event type is not a valid option. Please utilize \"event_types\", without capitalizing")
  }

  if (hasArg("Start_date")) {
    stop("Start_date is not a valid option. Please utilize \"start_date\", without capitalizing")
  }

  if (hasArg("End_date")) {
    stop("End_date is not a valid option. Please utilize \"end_date\", without capitalizing")
  }

  if (!population %in% c("none", "best", "full")) {
    stop("The 'population' argument must be one of 'none', 'best', or 'full'.")
  }

  # Error checks for arguments ----

  if (!is.character(email) || is.null(email) || (is.character(email) && nchar(email) < 3)) {
    stop("Email address required for ACLED API access. 'email' must be a character string (e.g., 'name@mail.com') or a call to where your email address is located if stored as an environment variable (e.g., Sys.getenv('acled_email'). Register your email for access at https://developer.acleddata.com.")
  }
  email_internal <- paste0("&email=", email)

  if ((!is.character(key) || is.null(key) || key == "") == TRUE) {
    stop("Key required for ACLED API access. 'key' must be a character string (e.g., 'xyz123!etc') or a call to where your ACLED API key is located if stored as an environment variable (e.g., Sys.getenv('acled_key'). Request and locate your ACLED API key at https://developer.acleddata.com.")
  }
  key_internal <- paste0("&key=", key)

  if (!is.null(country) & sum(unique(country) %in% acledR::acled_countries[["country"]]) < length(unique(country))) {
    stop("One or more of the requested countries are not in ACLED's countries list. The full list of countries is available at 'acledR::acled_countries")
  }

  # Checking if regions are input incorrectly ----
  if (is.character(regions) & sum(unique(regions) %in% acledR::acled_regions[["region_name"]]) < length(unique(regions))) {
    stop("One or more requested region names not in the ACLED country list. The full list of ACLED regions is available at 'acledR::acled_regions'.")
  }
  if (is.numeric(regions) & sum(unique(regions) %in% acledR::acled_regions[["region"]]) < length(unique(regions))) {
    stop("One or more requested region numbers not in the ACLED country list. The full list of ACLED regions is available at 'acledR::acled_regions'.")
  }

  if(!population %in% c("none", "best", "full")) {
    stop("The 'population' argument must be one of 'none', 'best', or 'full'.")
  }

  # Required components ----
  base_url <- "https://api.acleddata.com/acled/read.csv?"

  # Calculate country days ----

  # Setup base data to check how many country-days are being requested
  if (!is.null(country) & is.null(regions)) {
    test <- country


    df <- acledR::acled_countries %>%
      filter(.data$country %in% test)

    # Subset acled_multipliers (subset is faster than filter in our case) by relevant country & year
    ex1_df <- subset(acledR::acled_multipliers, country %in% test, select = country:avg_month_bin)
    ex1_df <- subset(ex1_df, year <= lubridate::year(end_date) & year >= lubridate::year(start_date))
  } else if (is.null(country) & !is.null(regions)) {
    if (is.numeric(regions)) {
      regions <- acledR::acled_regions %>%
        filter(.data$region %in% regions) %>%
        pull(.data$region_name)
    }

    df <- acledR::acled_countries %>%
      filter(.data$region %in% regions)

    ex1_df <- subset(acledR::acled_multipliers, country %in% unique(df$country), select = country:avg_month_bin)
    ex1_df <- subset(ex1_df, year <= lubridate::year(end_date) & year >= lubridate::year(start_date))
  } else if (!is.null(country) & !is.null(regions)) {
    if (is.numeric(regions)) {
      regions <- acledR::acled_regions %>%
        filter(.data$region %in% regions) %>%
        pull(.data$region_name)
    }

    test <- country

    df <- acledR::acled_countries %>%
      filter((.data$country %in% test) | (.data$region %in% regions))

    ex1_df <- subset(acledR::acled_multipliers, country %in% unique(df$country), select = country:avg_month_bin)
    ex1_df <- subset(ex1_df, year <= lubridate::year(end_date) & year >= lubridate::year(start_date))
  } else {
    df <- acledR::acled_countries
    ex1_df <- subset(acledR::acled_multipliers, country %in% unique(df$country), select = country:avg_month_bin)
    ex1_df <- subset(ex1_df, year <= lubridate::year(end_date) & year >= lubridate::year(start_date))
  }

  # Not checking unit test below as it is a non-critical feature, as start_date is no longer NULL by default.
  if (is.null(start_date)) { # nocov start
    start_date_check <- "1997-01-01"
  } # nocov end
  else {
    start_date_check <- start_date
  }

  if (is.null(end_date)) {
    end_date_check <- Sys.Date()
  } else {
    end_date_check <- end_date
  }

  # Inject

  days_per_year <- function(sd, ed) {
    # Convert to Date objects
    start <- as.Date(start_date)
    end <- as.Date(end_date)

    # Identify the years in the range
    years <- seq(year(start), year(end))

    # Calculate days for each year
    days_in_each_year <- sapply(years, function(y) {
      start_of_year <- as.Date(paste0(y, "-01-01"))
      end_of_year <- as.Date(paste0(y, "-12-31"))

      current_start <- ifelse(start_of_year < start, start, start_of_year)
      current_end <- ifelse(end_of_year > end, end, end_of_year)

      as.numeric(current_end - current_start + 1) # +1 to make the end_date inclusive
    })

    names(days_in_each_year) <- years
    return(days_in_each_year)
  }

  object <- days_per_year(start_date_check, end_date_check)

  ex1_df <- ex1_df %>%
    mutate(
      # Add n_days_requested based of the days_per_year result
      n_days = object[as.character(year)],
      # Devide avg_month_bins into days, because not every call will be about months
      avg_daily_bin = avg_month_bin / 30,
      # Multiply the avg_daily_bin with the number of days
      ee_events = avg_daily_bin * n_days
    )


  out <- df %>%
    mutate(
      t_start = lubridate::as_date(start_date_check),
      t_end = lubridate::as_date(end_date_check),
      t_start = case_when(
        as.numeric(lubridate::year(t_start)) < start_year ~ lubridate::as_date(paste0(start_year, "-01-01")),
        TRUE ~ t_start
      ),
      time = .data$t_end - .data$t_start
    )

  n_countries <- length(unique(out$country))



  # Note for how much data is being requested
  size_note <- paste(
    "Requesting data for",
    length(unique(ex1_df$country)),
    ifelse(length(unique(ex1_df$country)) == 1, "country", "countries"),
    "from", start_date, "to", end_date
  )

  message(size_note)


  # Current ceilling 400k
  time_units <- ceiling(sum(ex1_df$ee_events) / 400000)

  # Split call into roughly equally sized groups depending on how many country-days are in each country
  # This randomly assigns country into bins
  out_groups <- split(out, sample(1:time_units, nrow(out), replace = T))

  if (log == T) {
    if (length(out_groups) > 1) {
      log_rep <- map_dfr(out_groups, bind_rows, .id = "id") %>%
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
  if (!is.null(start_date) & !is.null(end_date)) {
    dates_internal <- paste0("&event_date=", paste(start_date, end_date, sep = "|"), "&event_date_where=BETWEEN")
  }


  if (!is.null(start_date) & !is.null(end_date)) {
    if (start_date > end_date) {
      stop("Requested 'start_date' is after the requested 'end_date'.")
    }
  }


  # Where
  ## country

  countries_internal <- vector("list", length = length(out_groups))
  for (i in 1:length(out_groups)) {
    countries_internal[[i]] <- paste0("&country=", paste(gsub("\\s{1}", "%20", out_groups[[i]]$country), collapse = ":OR:country="))
    countries_internal[[i]] <- paste0(countries_internal[[i]], "&country_where=%3D")
  }


  # Timestamps
  if (!is.null(timestamp)) {
    timestamp_into_date <- tryCatch(
      {
        lubridate::ymd(timestamp)

        timestamp_into_date <- "string"
      },
      warning = function(w) {
        a <- "numerical"
      },
      error = function(e) {
        a <- "numerical"
      }
    )

    if (timestamp_into_date == "string") {
      timestamp_parsable <- lubridate::ymd(timestamp)
      do_i_include_timestamp <- "Yes"
    } else {
      timestamp_parsable <- tryCatch(
        {
          lubridate::date(lubridate::as_datetime(timestamp))
          do_i_include_timestamp <- "Yes_but_numerical"
        },
        warning = function(w) {
          za <- menu(c("Yes", "No"),
            title = paste0("You indicated a timestamp, but it was not recognized. Reminder: Timestamp only accepts string as yyyy-mm-dd OR a Unix timestamp (integer).", "\n", "\n", "Do you want me to continue and ignore timestamp?")
          )
          if (za == 1) {
            do_i_include_timestamp <<- "No"
          } else {
            stop("User requested to abort when timestamp was not recognized.")
          }
        },
        error = function(e) {
          stop("User requested to abort when timestamp was not recognized.")
        }
      )
    }

    if (do_i_include_timestamp == "Yes") {
      if (timestamp_parsable > lubridate::now()) {
        stop("The timestamp cannot be later than today. Please change the timestamp and try again.")
      } else {
        timestamp_internal <- paste0("&timestamp=", timestamp_parsable)
      }
    } else if (do_i_include_timestamp == "Yes_but_numerical") {
      timestamp_internal <- paste0("&timestamp=", timestamp)
    } else {
      timestamp_internal <- "&timestamp="
    }
  } else {
    timestamp_internal <- "&timestamp="
  }

  # How
  if (isTRUE(monadic)) {
    monadic_internal <- "&export_type=monadic"
  } else {
    monadic_internal <- ""
  }

  # Event types
  if (!is.null(event_types)) {
    event_types <- str_to_upper(event_types)
    if (FALSE %in% unique(event_types %in% str_to_upper(c(
      "Battles", "Violence against civilians", "Protests",
      "Riots", "Strategic Developments", "Explosions/Remote violence"
    )))) {

      stop("One or more requested event types are not in the ACLED data. Event types include: Battles, Violence against civilians, Protests, Riots, Strategic Developments, and Explosions/Remote violence. Leave 'event_type = NULL' to request all event types from the API. ")
    }

    event_types_internal <- paste0("&event_type=", paste(gsub("\\s{1}", "%20", event_types), collapse = ":OR:event_type="))
  } else {
    event_types_internal <- ""
  }


  # Interactive choice for users after prompting how many calls are required - I am nocov this one because of discrepancy between
  # covr, devtools and testthat. After testing with testthat and devtools::test() it shows that it works. But covr seems to fail.

  if (prompt == TRUE) { # nocov start

    message(paste0(
      "This request requires ",
      time_units,
      " API calls. Do you want to proceed with this request?\nIf you need to increase your API quota, please contact access@acleddata.com"
    ))

    if (interactive()) {
      user_input <- menu(title = "Proceed? (Yes/No)", choices = c("Yes", "No"))
      if (user_input == 2) {
        stop('User responded "No" when prompted about the number of API calls required. \nIf you need to increase your API quota, please contact access@acleddata.com',
          call. = F
        )
      } else {
        message(
          "Proceeding with ",
          time_units,
          " API calls"
        )
      }
    }
  } # nocov end



  # Population argument

  if(population == "none") {
    population_internal <- ""
  } else if (population == "best") {
    population_internal <- "&population=true"
  } else {
    population_internal <- "&population=full"
  }

  # Inter argument

  if(inter_numeric == TRUE) {
    inter_internal <- "&inter_num=1"
  } else {
    inter_internal <- "&inter_num=0"
  }

  # Loop through country bins to define each api call
  url_internal <- vector("list", length = length(out_groups))
  for(i in 1:length(out_groups)) {
    url_internal[[i]] <- paste0(base_url, monadic_internal,
                                email_internal, key_internal,
                                countries_internal[[i]],
                                dates_internal, timestamp_internal,
                                event_types_internal, population_internal,
                                inter_internal, ..., "&limit=0")
  }


  # Loop through the api requests
  response <- vector("list", length = length(out_groups))
  message("Processing API request")
  for (i in 1:length(out_groups)) {
    response[[i]] <- httr::GET(url_internal[[i]])

    if (response[[i]][["status_code"]] == 500) {
      stop(paste0("API request unsuccessful with status code ", response[[i]][["status_code"]], ". \n", rlang::format_error_bullets(c("Make sure you have not execeeded your API calls (2/year for a standard account)", "Verify your API credentials (key and email)", "If nothing works contact us through GitHub Issues or at access@acleddata.com."))))
    } else if (response[[i]][["status_code"]] == 503 | response[[i]][["status_code"]] == 502) {
      stop(paste0("API request unsuccessful with status code ", response[[i]][["status_code"]], ". \n", "Our server may be under maintenance or it may momentarily be unavailable; please try again in a couple of minutes."))
    }
  }

  # Map through each get request to convert to one tibble
  message("Extracting content from API request")
  out <- suppressMessages(purrr::map_df(
    .x = response,
    ~ content(.x)
  ))


  return(out)
}
