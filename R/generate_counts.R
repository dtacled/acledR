#' Generate event counts from ACLED data
#'
#' @param data ACLED data
#' @param event_type Event types to include. If more than one event type is included, event counts per type and the total number of events is returned. If NULL, all event types are returned.
#' @param unit_id string. Unit variable (e.g., country, region, admin1, etc.).
#' @param time_id string. Temporal variable, usually event_date.
#' @param time_target string. Target temporal unit (e.g. week, month, year).
#' @param start_date Earliest date to include (yyyy-mm-dd).
#' @param end_date  Latest date to include (yyyy-mm-dd).
#' @param add_unit_ids Option to add in units with no events throughout the time period of interest.
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import janitor
#' @importFrom rlang .data
#' @return Returns a tibble grouped by unit_id.
#' @family Data Manipulation
#' @seealso
#' \itemize{
#' \item ACLED API guide. <https://acleddata.com/acleddatanew//wp-content/uploads/dlm_uploads/2021/11/API-User-Guide_Feb2022.pdf>
#' }
#' @examples
#' \dontrun{
#'
#' ### General example ###
#' # Request all events from a few countries
#' df_events <- acled_api(countries = c("Brazil", "Mexico", "Argentina"),
#'                        start_date = "2022-01-01",
#'                        end_date = "2022-07-30",
#'                        monadic = F,
#'                        acled_access = TRUE)
#'
#' # Generate event counts at the admin1-month level
#' df_counts <- generate_counts(data = df_events,
#'                              unit_id = "admin1",
#'                              time_id = "event_date",
#'                              time_target = "month")
#'
#'
#'
#' ### add_unit_ids example ###
#' # Request riots in the United States and Canada between January 1st and January 30th 2020
#' df_riots <- acled_api(countries = c("United States", "Canada"),
#'                        start_date = "2022-01-01",
#'                        end_date = "2022-01-30",
#'                        event_types = "Riots",
#'                        monadic = F,
#'                        acled_access = TRUE)
#'
#' # Notice that there are no riot events in Canada over this period
#' table(df_riots$country)
#'
#' # Generate weekly riot counts
#' # Use the add_unit_ids paramater so Canada is assigned 0 events each week
#' # If add_unit_ids was not included, only weekly counts for the United States
#' # would be returned since Canada had no riots in the requested time period
#' df_counts_riots <- generate_counts(data = df_riots,
#'                                    unit_id = "country",
#'                                    event_type = "Riots",
#'                                    time_id = "event_date",
#'                                    time_target = "week",
#'                                    add_unit_ids = "Canada")
#'
#' }
#' @md
#'
#' @importFrom rlang .data
#' @importFrom data.table :=
#' @importFrom tidyselect where
#' @export
#'

generate_counts <-
  function(data, event_type = NULL,
           unit_id, time_id, time_target,
           start_date = NULL, end_date = NULL,
           add_unit_ids = NULL) {

    if(!is.null(event_type))
      if(sum(event_type %in% unique(data[["event_type"]])) < length(event_type))
        stop("One or more requested event types not in data.")

    if(is.null(start_date))
      start_date <- min(data[["event_date"]])

    if(is.null(end_date))
      end_date <- max(data[["event_date"]])


    if(min(data[["event_date"]]) > as.Date(end_date))
      stop("Earliest event date in data is after the requested end_date.")

    if(max(data[["event_date"]]) < as.Date(start_date))
      stop("Latest event date in data is before requested start_date.")

    if(min(data[["event_date"]]) > as.Date(start_date))
      warning("Requested start_date is before the earliest date in the data. Returning only dates in the requested range that are within the temporal bounds of the data.")

    if(max(data[["event_date"]]) < as.Date(end_date))
      warning("Requested end_date is after the latest date in the data. Returning only dates in the requested range that are within the temporal bounds of the data.")





    all_dates <- seq(floor_date(as.Date(start_date), time_target,
                                week_start = getOption('lubridate.week.start', 6)),
                     floor_date(as.Date(end_date), time_target,
                                week_start = getOption('lubridate.week.start', 6)),
                     by = time_target)



    if(is.null(event_type)) {
      filter_types <- unique(data[["event_type"]])
    } else {
      filter_types <- event_type
    }



    if(is.null(add_unit_ids)){
      add_unit_ids <- unique(data[[unit_id]])
    } else {
      add_unit_ids <- unique(c(unique(data[[unit_id]]), add_unit_ids))
    }


    data <-
      data %>%
      filter(event_type %in% filter_types) %>%
      mutate(event_date = ymd(.data[[time_id]]),
             event_time = floor_date(.data$event_date, time_target, week_start = getOption('lubridate.week.start', 6))) %>%
      filter(between(.data$event_time, as.Date(start_date), as.Date(end_date))) %>%

      group_by(.data[[unit_id]], .data$event_time, event_type) %>%

      summarise(count = n()) %>%
      ungroup() %>%
      full_join(merge(add_unit_ids, all_dates) %>%
                  as_tibble() %>%
                  rename({{unit_id}} := .data$x, event_time =.data$y)) %>%
      mutate(count = case_when(is.na(count) ~ as.numeric(0),
                               TRUE ~ as.numeric(count))) %>%
      rename(!!paste0("event_", time_target) := .data$event_time) %>%
      pivot_wider(values_from = count, names_from = event_type) %>%
      mutate(across(.cols = where(is.numeric), ~case_when(is.na(.) ~ 0, TRUE ~ .))) %>%
      rowwise() %>%
      mutate(total_events = sum(c_across(where(is.numeric)))) %>%
      janitor::clean_names() %>%
      ungroup() %>%
      select(-one_of("na")) %>%
      arrange(.data[[unit_id]], !!paste0("event_", time_target)) %>%
      suppressMessages() %>%
      suppressWarnings()


    if(length(filter_types) == 1)
      data <- data %>% select(-.data$total_events)

    return(data)



  }

