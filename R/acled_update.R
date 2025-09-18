#' @title Updating your ACLED dataset
#' @name acled_update
#' @description
#' This function is meant to help you keep your dataset updated, by automatically checking for new and modified events, as well as deleted events (if deleted = TRUE).
#' Note: The function makes new API calls to gather new and modified events.
#' @param df The dataframe to update, it has to have the same structure as ACLED's dyadic dataframe (i.e. the result of `acled_api()`)
#' @param start_date The first date of events you want to update from.. These are the celling and floor of *event_date*, not of *timestamp*.
#' @param end_date The last date of events you want to update from. These are the celling and floor of *event_date*, not of *timestamp*.
#' @param additional_countries string. Additional additional_countries to update your dataset. It defaults to “current countries”, which includes all the additional_countries inside your dataset.
#' @param regions string. The regions for which you would like events in your dataset updated.
#' @param event_types string. The event types for which you would like events in your dataset updated.
#' @param email character string. Email associated with your ACLED account registered at <https://acleddata.com/>.
#' @param password character string. The password associated with your ACLED account. If NULL, you will be prompted to enter your password interactively.
#' @param deleted logical. If TRUE (default), the function will also remove deleted events using acled_deletions_api().
#' @param inter_numeric logical. If FALSE (default), interaction code columns (inter1, inter2, and interaction) returned as strings describing the actor types/interactions. If TRUE, the values are returned as numeric values. Must match the inter type (numeric or string) in the dataframe being updated.
#' @return Tibble with updated ACLED data and a newer timestamp.
#' @family API and Access
#' @examples
#' \dontrun{
#' # Updating dataset to include newer data from Argentina
#'
#'
#' new_argen_dataset <- acled_update(acledR::acled_old_dummy,
#'  email = "youremail@mail.com", password = "password",
#'   additional_countries = "Argentina",
#' )
#' }
#'
#' @md
#' @importFrom dplyr filter
#' @importFrom dplyr anti_join
#' @importFrom methods hasArg
#'
#' @export


# acled_update
acled_update <- function(df,
                         start_date = min(df$event_date),
                         end_date = max(df$event_date),
                         additional_countries = "current countries",
                         regions = NULL,
                         event_types = NULL,
                         email = NULL,
                         password = NULL,
                         inter_numeric = FALSE,
                         deleted = TRUE) {


  if (is.null(email)) {
    stop("Email not specified. You must include the email associated with your ACLED account in the `email` parameter.")
  }

  if (is.null(password)) {
    stop("Password not specified. You must include the email associated with your ACLED account in the `password` parameter.")
  }




  if (!setequal(colnames(df), colnames(acledR::acled_old_deletion_dummy))) {
    stop("The data frame provided does not have ACLED's structure. Please make sure the data frame provided follows the same structure.")
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

  if (inter_numeric == FALSE & any(1:8 %in% unique(df$inter1))) {
    stop("The data frame provided appears to have numeric interaction values (inter1, inter2, and interaction variables). Set inter_numeric = TRUE in the acled_update() call to update data with numeric interaction values.")
  }


  if (all(additional_countries == "current countries")) {
    additional_countries <- unique(df$country)
  } else {
    additional_countries <- append(unique(df$country), additional_countries)
  }


  # Check event_types
  if (!is.null(event_types)) {
    valid_event_types <- acledR::acled_event_categories$event_type
    if (!all(event_types %in% valid_event_types)) {
      stop("Error: Invalid event_type provided. Please use an event type present in ACLED's methodology.")
    }
  }

  # Error check for additional_countries
  if (!all(additional_countries %in% acledR::acled_countries$country)) {
    missing_countries <- additional_countries[!(additional_countries %in% acledR::acled_countries$country)]
    stop(paste(
      "Error: The following additional_countries are not present in acledR::acled_countries:",
      paste(missing_countries, collapse = ", ")
    ))
  }

  # Error check for regions
  if (!all(regions %in% acledR::acled_regions$region_name)) {
    missing_regions <- regions[!(regions %in% acledR::acled_regions$region_name)]
    stop(paste(
      "Error: The following regions are not present in acledR::acled_regions:",
      paste(missing_regions, collapse = ", ")
    ))
  }


  max_timestamp <- max(df$timestamp)

  if (deleted == TRUE ) {
    deleted_events <- acled_deletions_api(email = email, password = password, date_deleted = max_timestamp)

    after_deleted <- df %>%
      filter(!(df$event_id_cnty %in% deleted_events$event_id_cnty))
  } else {
    after_deleted <- df
  }

  new_dataset <- acled_api(
    email = email,
    password = password,
    start_date = start_date,
    end_date = end_date,
    country = additional_countries,
    regions = regions,
    event_types = event_types,
    timestamp = max_timestamp,
    inter_numeric = inter_numeric
  )

  updated_dataset <- after_deleted %>%
    anti_join(new_dataset, by = "event_id_cnty") %>%
    rbind(new_dataset)

  if (deleted == TRUE) {
    message(paste0(
      "Dataset updated. \n Old number of events: ", nrow(df),
      ". \n New events: ", nrow(updated_dataset) - nrow(df),
      ". \n Deleted events: ", nrow(df) - nrow(after_deleted),
      ". \n Total new & modified events: ", nrow(new_dataset)
    ))
  } else {
    message(paste0(
      "Dataset updated. \n Old number of events: ", nrow(df),
      ". \n New events: ", nrow(updated_dataset) - nrow(df),
      ". \n Total new & modified events: ", nrow(new_dataset)
    ))
  }
  return(updated_dataset)
}
