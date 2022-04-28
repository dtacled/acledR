#' Generate event counts from ACLED data
#'
#' @param data ACLED data
#' @param event_type Event types to include
#' @param unit_id Unit variable
#' @param time_id Temporal variable
#' @param time_target Target temporal unit
#' @param start_date Earliest date to include
#' @param end_date  Latest date to include
#' @param add_unit_ids Option to add in units with no events at certain time periods
#' @return Returns a tibble grouped by unit_id
#'
#'
#'
#' @export
generate_counts <-
  function(data, event_type = NULL, unit_id, time_id, time_target,
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

    if(!is.null(add_unit_ids))
      add_unit_ids <- unique(data[[unit_id]])
    else(add_unit_ids <- c(unique(data[[unit_id]], add_unit_ids)))


    data %>%
      filter(event_type %in% filter_types) %>%
      filter(between(event_date, as.Date(start_date), as.Date(end_date))) %>%
      mutate(event_date = ymd(.data[[time_id]]),
             event_time = floor_date(event_date, time_target, week_start = getOption('lubridate.week.start', 6))) %>%

      group_by(.data[[unit_id]], event_time) %>%

      summarise(count = n()) %>%
      ungroup() %>%
      full_join(merge(add_unit_ids, all_dates) %>%
                  as_tibble() %>%
                  rename(country = x, event_time = y)) %>%
      mutate(count = case_when(is.na(count) ~ as.numeric(0),
                               TRUE ~ as.numeric(count))) %>%
      rename(!!paste0("event_", time_target) := event_time) %>%
      suppressMessages()



  }

