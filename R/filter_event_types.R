#' Filter ACLED data by event type categories
#' @title acled_filter_event_types
#'
#' @param data ACLED event data
#' @param acled_categories character vector of ACLED event type categories. Options include: political_violence, organized_political_violence, disorder, and demonstrations. More than one category may be requested at one time (e.g., acled_categories = c("organized_political_violence", "disorder")).
#' @param keep_all_events logical. If FALSE (default), the returned data will include only events within the requested categories. If TRUE, the returned data will retain all events, even those that do not fall within the requested categories. New dummy columns for each requested category denote which categories each event type falls within.
#' @import dplyr
#' @return Returns a tibble of ACLED events and new columns (dummy variables) indicating which categories each event falls under. If `keep_all_events = FALSE`, only events that fall within one of the requested categories are returned. If `keep_all_events = TRUE`, all events, including those not within a requested category, are returned.
#'
#' @family Data Manipulation
#' @examples
#' \dontrun{
#'
#' # Request data for 3 countries from the ACLED API
#' df_events <- acled_api(countries = c("Nigeria", "Benin", "Senegal"),
#'                        start_date = "2022-01-01",
#'                        end_date = "2022-09-30",
#'                        monadic = F,
#'                        acled_access = TRUE)
#'
#' # Filter to only ACLED demonstrations
#' df_demonstrations <- filter_event_types(data = df_events, acled_categories = c("demonstrations"))
#'
#' # This returns a tibble with only peaceful protests, protests with intervention,
#' # violence demonstrations, and excessive force against protesters.
#' df_demonstrations
#'
#' # Users may also filter by multiple event categories
#' df_demonstrations_disorder <- filter_event_types(df_events,
#'                                                  c("demonstrations", "disorder"))
#'
#' # Which returns all rows that are either demonstrations and/or disorder events
#' df_demonstrations_disorder
#'
#' }
#' @md
#' @importFrom rlang .data
#' @export

acled_filter_event_types <- function(data,
                               acled_categories,
                               keep_all_events = FALSE) {

  if("event_type" %in% colnames(data) == FALSE) {
    stop("Column 'event_type' not in data. Please use data with columns for both ACLED 'event_type' and 'sub_event_type'.")
  }

  if("sub_event_type" %in% colnames(data) == FALSE) {
    stop("Column 'event_type' not in data. Please use data with columns for both ACLED 'event_type' and 'sub_event_type'.")
  }


  if(FALSE %in% unique(acled_categories %in% c("political_violence", "organized_political_violence", "disorder", "demonstrations"))) {
    stop("Requested 'acled_category' is not a designated option. Valid options include: political_violence, organized_political_violence, disorder, and demonstrations.")
  }

  if(isFALSE(keep_all_events)) {
    out_data <- data %>%
      left_join(select(acledR::acled_event_categories,event_type,sub_event_type,acled_categories), by=c("event_type", "sub_event_type")) %>%
      filter(if_any(acled_categories, ~.x > 0)) %>%
      ungroup()
  }
  else {
    out_data <- data %>%
      left_join(select(acledR::acled_event_categories,event_type,sub_event_type,acled_categories),by=c("event_type", "sub_event_type")) %>%
      ungroup()

  }

  return(out_data)

}

