#' @title Updating your ACLED dataset
#' @description
#' This function is meant to help you keep your dataset updated, by automatically checking for new and modified events, as well as deleted events (if deleted = TRUE).
#' Note: The function makes new api calls to gather new and modified events. See its vignettes in: <https://acled.github.io/acledR/articles/acled_deletions_api.html>
#
#' @param countries string. The countries by which to filter.
#' @param regions string. The regions by which to filter.
#' @param event_types string. The event types by which to filter.
#' @param acled_access logical. If TRUE (default), you have used the acled_access function and the email and key arguments are not required.
#' @param email character string. Email associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param key character string. Access key associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param deleted logical. If TRUE (default), the function will also remove deleted events using acled_deletions_api().
#' @param prompt logical. If TRUE (default), users will receive an interactive prompt providing information about their call (countries requested, number of country-days, and number of API calls required) and asking if they want to proceed with the call. If FALSE, the call continues without warning, but the call is split and returns a message specifying how many calls are being made.
#' @family API and Access
#' @importFrom dplyr filter
#' @importFrom dplyr anti_join
#' @importFrom methods hasArg
#' @export
#' @md

# acled_update
acled_update <- function(df,
                         start_date = min(df$event_date),
                         end_date = max(df$event_date),
                         countries = NULL,
                         regions = NULL,
                         event_types = NULL,
                         acled_access = TRUE,
                         email = NULL,
                         key = NULL,
                         deleted = TRUE,
                         prompts = T) {

  if(!setequal(colnames(df), colnames(acledR::acled_old_deletion_dummy))){
    stop("The data frame provided does not have ACLED's structure. Please make sure the data frame provided follows the same structure.")
  }

  if(hasArg("country") | hasArg("Country")){
    stop("Country is not a valid option. Please utilize \"countries\"")

  }

  if(hasArg("Countries")){
    stop("Countries is not a valid option. Please utilize \"countries\", without capitalizing")

  }

  if(hasArg("region")|hasArg("Region")){
    stop("Region is not a valid option. Please utilize \"regions\"")

  }

  if(hasArg("Regions")){
    stop("Regions is not a valid option. Please utilize \"regions\", without capitalizing")

  }

  if(hasArg("event_type")){
    stop("event type is not a valid option. Please utilize \"event_types\"")

  }

  if(hasArg("Event_type")){
    stop("Event type is not a valid option. Please utilize \"event_types\", without capitalizing")

  }

  if(hasArg("Start_date")){
    stop("Start_date is not a valid option. Please utilize \"start_date\", without capitalizing")

  }

  if(hasArg("End_date")){
    stop("End_date is not a valid option. Please utilize \"end_date\", without capitalizing")

  }

  if (start_date < min(df$event_date)) {
    warning("Warning: Start date is earlier than the earliest event date in your dataframe.")
  }

  if (start_date > min(df$event_date)) {
    warning("Warning: Start date is later than the earliest event date in your dataframe.")
  }

  if (end_date > max(df$event_date)) {
    warning("Warning: End date is later than the latest event date in your dataframe.")
  }

  if (end_date < max(df$event_date)) {
    warning("Warning: End date is earlier than the latest event date in your dataframe.")
  }

  # Check acled_access
  if (!acled_access && (is.null(email) || is.null(key))) {
    stop("Error: If acled_access is FALSE, you must provide an email and key.")
  }

  # Check event_types
  if (!is.null(event_types)) {
    valid_event_types <- acledR::event_categories$event_type
    if (!all(event_types %in% valid_event_types)) {
      stop("Error: Invalid event_type provided. Please use an event type present in ACLED's methodology.")
    }
  }

  # Error check for countries
  if(!all(countries %in% acledR::acled_countries$country)) {
    missing_countries <- countries[!(countries %in% acledR::acled_countries$country)]
    stop(paste("Error: The following countries are not present in acledR::acled_countries:",
               paste(missing_countries, collapse = ", ")))
  }

  # Error check for regions
  if(!all(regions %in% acledR::acled_regions$region_name)) {
    missing_regions <- regions[!(regions %in% acledR::acled_regions$region_name)]
    stop(paste("Error: The following regions are not present in acledR::acled_regions:",
               paste(missing_regions, collapse = ", ")))
  }


  max_timestamp <- max(df$timestamp)

  deleted_events <- acled_deletions_api(email = email, key = key, date_deleted = max_timestamp, acled_access = acled_access)

  after_deleted <- df %>%
    filter(!(event_id_cnty %in% deleted_events$event_id_cnty))

  new_dataset <- acled_api(email = email,
                           key = key,
                           start_date = start_date,
                           end_date = end_date,
                           countries = countries,
                           regions = regions,
                           event_types = event_types,
                           acled_access = acled_access,
                           timestamp = max_timestamp,
                           prompt = prompts,
                           )

  updated_dataset <- after_deleted %>%
    anti_join(new_dataset, by = "event_id_cnty") %>%
    rbind(new_dataset)


  message(paste0("Dataset updated. \n Old number of events: ",nrow(df),
                 ". \n New events: ", nrow(updated_dataset) - nrow(df),
                 ". \n Deleted events: ", nrow(df) - nrow(after_deleted),
                 ". \n Total new & modified events: ", nrow(new_dataset)))

  return(updated_dataset)
}
