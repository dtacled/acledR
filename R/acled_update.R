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
