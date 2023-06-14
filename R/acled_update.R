#' @title Updating your ACLED dataset
#' @name acled_update
#' @description
#' This function is meant to help you keep your dataset updated, by automatically checking for new and modified events, as well as deleted events (if deleted = TRUE).
#' Note: The function makes new api calls to gather new and modified events. See its vignettes in: <https://acled.github.io/acledR/articles/acled_deletions_api.html>
#' @param df ACLED dataset that needs to be updated (i.e. a result from acledR::acled_api())
#' @param start_date Start date from which to check updates in the dataset (i.e. the minimum date in your dataset)
#' @param end_date End date from which to check updates in the dataset (i.e., the latest date in your dataset)
#' @param countries string. The countries by which to filter.
#' @param regions string. The regions by which to filter.
#' @param event_types string. The event types by which to filter.
#' @param acled_access logical. If TRUE (default), you have used the acled_access function and the email and key arguments are not required.
#' @param email character string. Email associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param key character string. Access key associated with your ACLED account registered at <https://developer.acleddata.com>.
#' @param deleted logical. If TRUE (default), the function will also remove deleted events using acled_deletions_api().
#' @param prompts logical. If TRUE (default), users will receive an interactive prompt providing information about their call (countries requested, number of country-days, and number of API calls required) and asking if they want to proceed with the call. If FALSE, the call continues without warning, but the call is split and returns a message specifying how many calls are being made.
#' @return Tibble with updated ACLED data and a newer timestamp.
#' @family API and Access
#' @seealso
#' \itemize{
#' \item ACLED Keeping your dataset updated guide. <https://acleddata.com/download/35179/>
#' }
#' @examples
#' # Updating dataset to include newer data from Argentina
#'
#' acledR::acled_access(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5")
#'
#' new_argen_dataset <- acled_update(acledR::acled_old_dummy,
#'                                   countries = "Argentina",
#'                                   acled_access = TRUE,
#'                                   prompts = FALSE)
#'
#' @md
#' @importFrom dplyr filter
#' @importFrom dplyr anti_join
#' @importFrom methods hasArg
#' @export


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
                         prompts = T) { ## This is added for the hasArg statements to work. Not sure why it doenst work without it.

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

  if(!setequal(colnames(df), colnames(acledR::acled_old_deletion_dummy))){
    stop("The data frame provided does not have ACLED's structure. Please make sure the data frame provided follows the same structure.")
  }

  # Check event_types
  if (!is.null(event_types)) {
    valid_event_types <- acledR::acled_event_categories$event_type
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

  if(deleted == TRUE) {
    deleted_events <- acled_deletions_api(email = email, key = key, date_deleted = max_timestamp, acled_access = acled_access)

    after_deleted <- df %>%
      filter(!(event_id_cnty %in% deleted_events$event_id_cnty))
  } else {
    after_deleted <- df
  }


  new_dataset <- acled_api(email = email,
                           key = key,
                           start_date = start_date,
                           end_date = end_date,
                           countries = countries,
                           regions = regions,
                           event_types = event_types,
                           acled_access = acled_access,
                           timestamp = max_timestamp,
                           prompt = prompts)

  updated_dataset <- after_deleted %>%
    anti_join(new_dataset, by = "event_id_cnty") %>%
    rbind(new_dataset)

  if(deleted == TRUE){
    message(paste0("Dataset updated. \n Old number of events: ",nrow(df),
                   ". \n New events: ", nrow(updated_dataset) - nrow(df),
                   ". \n Deleted events: ", nrow(df) - nrow(after_deleted),
                   ". \n Total new & modified events: ", nrow(new_dataset)))
  } else {
    message(paste0("Dataset updated. \n Old number of events: ",nrow(df),
                   ". \n New events: ", nrow(updated_dataset) - nrow(df),
                   ". \n Total new & modified events: ", nrow(new_dataset)))
  }
  return(updated_dataset)
}
