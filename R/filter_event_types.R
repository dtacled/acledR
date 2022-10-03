#' Filter ACLED data by event type categories
#'
#' @param data ACLED data
#' @param acled_categories character vector of ACLED event type categories. Options include: political_violence, organized_political_violence, disorder, and demonstrations. More than one category may be requested at one time (e.g., acled_categories = c("organized_political_violence", "disorder")).
#' @import dplyr
#' @return Returns a tibble including only ACLED events that fall within the requested event categories, including new columns (dummy variables) indicating which categories each event falls under.
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
#' # This returns a tibble with only peaceful protests, protests with intervention, violence demonstrations, and excessive force against protesters.
#' df_demonstrations
#'
#' # Users may also filter by multiple event categories
#' df_demonstrations_disorder <- filter_event_types(data = df_events, acled_categories = c("demonstrations", "disorder"))
#'
#' # Which returns all rows that are either demonstrations and/or disorder events
#' df_demonstrations_disorder
#'
#' }
#' @md
#'
#'
#' @export

filter_event_types <- function(data, acled_categories) {

  if("event_type" %in% colnames(data) == FALSE) {
    stop("Column 'event_type' not in data. Please use data with columns for both ACLED 'event_type' and 'sub_event_type'.")
  }

  if("sub_event_type" %in% colnames(data) == FALSE) {
    stop("Column 'event_type' not in data. Please use data with columns for both ACLED 'event_type' and 'sub_event_type'.")
  }


  if(FALSE %in% unique(acled_categories %in% c("political_violence", "organized_political_violence", "disorder", "demonstrations"))) {
    stop("Requested 'acled_category' is not a designated option. Valid options include: political_violence, organized_political_violence, disorder, and demonstrations.")
  }

  out_data <- data %>%
    left_join(acledR::acled_event_categories %>%
                select(event_type, sub_event_type, acled_categories)) %>%
    filter(if_any(acled_categories, ~.x > 0)) %>%
    ungroup()

  return(out_data)

}

