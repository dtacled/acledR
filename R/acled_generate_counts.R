#' Generate event counts from ACLED data
#'
#' @param data data frame. ACLED data
#' @param event_type Event types to include. If more than one event type is included, event counts per type and the total number of events is returned. If NULL, all event types are returned.
#' @param unit_id string. Unit variable (e.g., country, region, admin1, etc.).
#' @param time_id string. Temporal variable, usually event_date.
#' @param time_target string. Target temporal unit (e.g. week, month, year).
#' @param start_date date. Earliest date to include (yyyy-mm-dd).
#' @param end_date  date. Latest date to include (yyyy-mm-dd).
#' @param fatalities_count logical. If TRUE, it will return fatalities counts INSTEAD of event counts.
#' @param add_unit_ids string. Option to add in units with no events throughout the time period of interest. To include hierarchical unique_id (e.g., country and admin1) add them separated by ";".
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import janitor
#' @importFrom rlang .data
#' @return Returns a tibble grouped by unit_id.
#' @family Data Analysis
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
#'                        monadic = FALSE,
#'                        acled_access = TRUE,
#'                        prompt = FALSE)
#'
#' # Generate event counts at the admin1-month level
#' df_counts <- acled_generate_counts(data = df_events,
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
#'                        monadic = FALSE,
#'                        acled_access = TRUE,
#'                        prompt = FALSE)
#'
#' # Notice that there are no riot events in Canada over this period
#' table(df_riots$country)
#'
#' # Generate weekly riot counts
#' # Use the add_unit_ids paramater so Canada is assigned 0 events each week
#' # If add_unit_ids was not included, only weekly counts for the United States
#' # would be returned since Canada had no riots in the requested time period
#'
#' df_counts_riots <- acled_generate_counts(data = df_riots,
#'                                    unit_id = "country",
#'                                    event_type = "Riots",
#'                                    time_id = "event_date",
#'                                    time_target = "week",
#'                                    fatalities_count = FALSE,
#'                                    add_unit_ids = "Canada")
#'
#' }
#' @md
#'
#' @importFrom rlang .data
#' @importFrom data.table :=
#' @importFrom tidyselect where
#' @importFrom methods hasArg
#' @export

acled_generate_counts <-
  function(data, event_type = NULL,
           unit_id, time_id, time_target,
           start_date = NULL, end_date = NULL,
           add_unit_ids = NULL,
           fatalities_count = FALSE) {

    if(!is.null(event_type))
      if(sum(event_type %in% unique(data[["event_type"]])) < length(event_type))
        stop("One or more requested event types not in data.")

    if(is.null(start_date))
      start_date <- min(data[["event_date"]])

    if(is.null(end_date))
      end_date <- max(data[["event_date"]])

    if(!hasArg(data)){
      stop("Error: data is missing. Indicate the dataframe from where to generate the counts.")
    }
    if(!hasArg(unit_id)){
      stop("Error: unit_id is missing. The unit_id is mandatory.")
    }
    if(!hasArg(time_id)){
      stop("Error: time_id is missing. The time_id is mandatory.")
    }
    if(!hasArg(time_target)){
      stop("Error: time_target is missing. The time_target is mandatory.")
    }

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

    # Adding unit ids
    to_add <- add_unit_ids

    if(!is.null(to_add)){

      if(str_count(to_add,";") < 1 & unit_id == "admin1") {
        stop("You requested counts by admin1, but the `add_unit_ids` argument contains only country. Please, add an admin1 to the `add_unit_ids` argument. \n
             If you are not interested in admin1 for your added unit, please add an empty space after `;`. E.g, `'Argentina; ' ")
      }

      if(str_count(to_add,";") > 1 & unit_id == "admin1") {
        stop("You requested counts by admin1, but the `add_unit_ids` argument contains more than one admin. There are more than one `;`. \n
             If you are interested in more than admin1 for your added unit, please change the `unit_id` argument or use a different separator than `;`.  ")
      }

      if(str_count(to_add,";") <2 & unit_id == "admin2") {
        stop("You requested counts by admin2, but the `add_unit_ids` argument contains only country and admin1. \n
             If you are not interested in admin2 for your added unit, please add an empty space after `;`. E.g, `'Argentina;La Rioja; ' ")
      }

      if(str_count(to_add,";") >3 & unit_id == "admin2") {
        stop("You requested counts by admin2, but the `add_unit_ids` argument contains admins beyond admin2. \n
             If you are interested in more than admin2 for your added unit, please change the `unit_id` argument or use a different separator than `;`. ")
      }

      if(str_count(to_add,";") <3 & unit_id == "admin3") {
        stop("You requested counts by admin3, but the `add_unit_ids` argument contains only country,admin1 and admin2. \n
             If you are not interested in admin3 for your added unit, please add an empty space after `;`. E.g, `'Argentina;La Rioja;Capital;  ' ")
      }
      if(str_count(to_add,";") >4 & unit_id == "admin3") {
        stop("You requested counts by admin3, but the `add_unit_ids` argument contains admins beyond admin3. \n
             If you are interested in more than admin3 for your added unit, please change the `unit_id` argument or use a different separator than `;`. ")
      }
      if(str_count(to_add,";") <4 & unit_id == "location") {
        stop("You requested counts by location, but the `add_unit_ids` argument contains only country,admin1, admin2 and admin3. \n
             If you are not interested in location for your added unit, please add an empty space after `;`. E.g, `'Argentina;La Rioja;Capital;NA; '.")
      }
      if(str_count(to_add,";") >5 & unit_id == "location") {
        stop("You requested counts by location, but the `add_unit_ids` argument contains admins beyond location.")
      }
    }

    # Generating unit_ids
      if(str_to_lower(unit_id) %in% c("country","region")){
        add_unit_ids <- data %>%
          select(.data[[unit_id]])%>%
          suppressWarnings() %>%
          distinct() %>%
          as.list()%>%
          .[[unit_id]]
      }
      if(str_to_lower(unit_id) == "admin1"){
        add_unit_ids <- data %>%
          select(.data$country,.data$admin1)%>%
          unite(for_merging,country,admin1, sep=";")%>%
          distinct() %>%
          as.list()%>%
          suppressWarnings() %>%
          .$for_merging
      }
      if(str_to_lower(unit_id) == "admin2"){
        add_unit_ids <- data %>%
          select(.data$country,.data$admin1,.data$admin2)%>%
          unite(for_merging,country,admin1,admin2, sep=";")%>%
          distinct() %>%
          as.list()%>%
          suppressWarnings() %>%
          .$for_merging
      }
      if(str_to_lower(unit_id) == "admin3"){
        add_unit_ids <- data %>%
          select(.data$country,.data$admin1,.data$admin2,.data$admin3)%>%
          unite(for_merging,country,admin1,admin2,admin3, sep=";")%>%
          distinct() %>%
          as.list()%>%
          suppressWarnings() %>%
          .$for_merging
      }
      if(str_to_lower(unit_id) == "location"){
        add_unit_ids <- data %>%
          select(.data$country, .data$admin1, .data$admin2, .data$admin3, .data$location)%>%
          unite(for_merging,country,admin1,admin2,admin3, location, sep=";")%>%
          distinct() %>%
          as.list()%>%
          suppressWarnings() %>%
          .$for_merging
      }


    if(!is.null(to_add)){
      add_unit_ids <- unique(c(add_unit_ids, to_add))
    }

    # Changing the start_date and end_date to avoid any missing months.

      start_date <- floor_date(ymd(start_date), unit = time_target,week_start = getOption('lubridate.week.start', 6))
      end_date <- floor_date(ymd(end_date), unit = time_target,week_start = getOption('lubridate.week.start', 6))

    if(fatalities_count == T){
      if(str_to_lower(unit_id) %in% c("country","region")){
        data <-
          data %>%
          filter(event_type %in% filter_types) %>%
          mutate(event_date = ymd(.data[[time_id]]),
                 event_time = floor_date(.data$event_date, time_target,week_start = getOption('lubridate.week.start', 6))) %>%
          filter(between(.data$event_time, as.Date(start_date), as.Date(end_date))) %>%

          group_by(.data[[unit_id]], .data$event_time, event_type) %>%

          summarise(fatalities = sum(.data$fatalities)) %>%
          ungroup() %>%
          full_join(merge(add_unit_ids, all_dates) %>%
                      as_tibble() %>%
                      rename({{unit_id}} := .data$x, event_time =.data$y)) %>%
          mutate(fatalities = case_when(is.na(fatalities) ~ as.numeric(0),
                                        TRUE ~ as.numeric(fatalities))) %>%
          rename(!!paste0("event_", time_target) := .data$event_time) %>%
          pivot_wider(values_from = fatalities, names_from = event_type) %>%
          mutate(across(.cols = where(is.numeric), ~case_when(is.na(.) ~ 0, TRUE ~ .))) %>%
          rowwise() %>%
          mutate(total_events = sum(c_across(where(is.numeric)))) %>%
          janitor::clean_names() %>%
          ungroup() %>%
          select(-one_of("na")) %>%
          arrange(.data[[unit_id]], !!paste0("event_", time_target)) %>%
          suppressMessages() %>%
          suppressWarnings()
      }
      if(str_to_lower(unit_id) == "admin1"){
        data <-
          data %>%
          filter(event_type %in% filter_types) %>%
          mutate(event_date = ymd(.data[[time_id]]),
                 event_time = floor_date(.data$event_date, time_target,week_start = getOption('lubridate.week.start', 6))) %>%
          filter(between(.data$event_time, as.Date(start_date), as.Date(end_date))) %>%

          group_by(.data$country,.data$admin1, .data$event_time, event_type) %>%

          summarise(fatalities = sum(.data$fatalities)) %>%
          ungroup() %>%
          full_join(merge(add_unit_ids, all_dates) %>%
                      as_tibble() %>%
                      separate(x,c("country","admin1"),sep=";") %>%
                      rename(event_time =.data$y))%>%
          mutate(fatalities = case_when(is.na(fatalities) ~ as.numeric(0),
                                      TRUE ~ as.numeric(fatalities))) %>%
          rename(!!paste0("event_", time_target) := .data$event_time) %>%
          pivot_wider(values_from = fatalities, names_from = event_type) %>%
          mutate(across(.cols = where(is.numeric), ~case_when(is.na(.) ~ 0, TRUE ~ .))) %>%
          rowwise() %>%
          mutate(total_events = sum(c_across(where(is.numeric)))) %>%
          janitor::clean_names() %>%
          ungroup() %>%
          select(-one_of("na")) %>%
          arrange(!!paste0("event_", time_target)) %>%
          suppressMessages() %>%
          suppressWarnings()
      }
      if(str_to_lower(unit_id) == "admin2"){
        data <-
          data %>%
          filter(event_type %in% filter_types) %>%
          mutate(event_date = ymd(.data[[time_id]]),
                 event_time = floor_date(.data$event_date, time_target,week_start = getOption('lubridate.week.start', 6))) %>%
          filter(between(.data$event_time, as.Date(start_date), as.Date(end_date))) %>%

          group_by(.data$country,.data$admin1, .data$admin2, .data$event_time, event_type) %>%

          summarise(fatalities = sum(.data$fatalities)) %>%
          ungroup() %>%
          full_join(merge(add_unit_ids, all_dates) %>%
                      as_tibble() %>%
                      separate(x,c("country","admin1","admin2"),sep=";") %>%
                      rename(event_time =.data$y))%>%
          mutate(fatalities = case_when(is.na(fatalities) ~ as.numeric(0),
                                      TRUE ~ as.numeric(fatalities))) %>%
          rename(!!paste0("event_", time_target) := .data$event_time) %>%
          pivot_wider(values_from = fatalities, names_from = event_type) %>%
          mutate(across(.cols = where(is.numeric), ~case_when(is.na(.) ~ 0, TRUE ~ .))) %>%
          rowwise() %>%
          mutate(total_events = sum(c_across(where(is.numeric)))) %>%
          janitor::clean_names() %>%
          ungroup() %>%
          select(-one_of("na")) %>%
          arrange(!!paste0("event_", time_target)) %>%
          suppressMessages() %>%
          suppressWarnings()
      }
      if(str_to_lower(unit_id) == "admin3"){
        data <-
          data %>%
          filter(event_type %in% filter_types) %>%
          mutate(event_date = ymd(.data[[time_id]]),
                 event_time = floor_date(.data$event_date, time_target,week_start = getOption('lubridate.week.start', 6))) %>%
          filter(between(.data$event_time, as.Date(start_date), as.Date(end_date))) %>%

          group_by(.data$country,.data$admin1, .data$admin2,.data$admin3, .data$event_time, event_type) %>%

          summarise(fatalities = sum(.data$fatalities)) %>%
          ungroup() %>%
          full_join(merge(add_unit_ids, all_dates) %>%
                      as_tibble() %>%
                      separate(x,c("country","admin1","admin2","admin3"),sep=";") %>%
                      rename(event_time =.data$y))%>%
          mutate(fatalities = case_when(is.na(fatalities) ~ as.numeric(0),
                                        TRUE ~ as.numeric(fatalities))) %>%
          rename(!!paste0("event_", time_target) := .data$event_time) %>%
          pivot_wider(values_from = fatalities, names_from = event_type) %>%
          mutate(across(.cols = where(is.numeric), ~case_when(is.na(.) ~ 0, TRUE ~ .))) %>%
          rowwise() %>%
          mutate(total_events = sum(c_across(where(is.numeric)))) %>%
          janitor::clean_names() %>%
          ungroup() %>%
          select(-one_of("na")) %>%
          arrange(!!paste0("event_", time_target)) %>%
          suppressMessages() %>%
          suppressWarnings()
      }
      if(str_to_lower(unit_id) == "location"){
        data <-
          data %>%
          filter(event_type %in% filter_types) %>%
          mutate(event_date = ymd(.data[[time_id]]),
                 event_time = floor_date(.data$event_date, time_target,week_start = getOption('lubridate.week.start', 6))) %>%
          filter(between(.data$event_time, as.Date(start_date), as.Date(end_date))) %>%

          group_by(.data$country,.data$admin1, .data$admin2,.data$admin3,.data$location, .data$event_time, event_type) %>%

          summarise(fatalities = sum(.data$fatalities)) %>%
          ungroup() %>%
          full_join(merge(add_unit_ids, all_dates) %>%
                      as_tibble() %>%
                      separate(x,c("country","admin1","admin2","admin3","location"),sep=";") %>%
                      rename(event_time =.data$y))%>%
          mutate(fatalities = case_when(is.na(fatalities) ~ as.numeric(0),
                                        TRUE ~ as.numeric(fatalities))) %>%
          rename(!!paste0("event_", time_target) := .data$event_time) %>%
          pivot_wider(values_from = fatalities, names_from = event_type) %>%
          mutate(across(.cols = where(is.numeric), ~case_when(is.na(.) ~ 0, TRUE ~ .))) %>%
          rowwise() %>%
          mutate(total_events = sum(c_across(where(is.numeric)))) %>%
          janitor::clean_names() %>%
          ungroup() %>%
          select(-one_of("na")) %>%
          arrange(!!paste0("event_", time_target)) %>%
          suppressMessages() %>%
          suppressWarnings()
      }
    } else {

    # Separating into options to avoid duplication of admins
    if(str_to_lower(unit_id) %in% c("country","region")){
      data <-
        data %>%
        filter(event_type %in% filter_types) %>%
        mutate(event_date = ymd(.data[[time_id]]),
               event_time = floor_date(.data$event_date, time_target,week_start = getOption('lubridate.week.start', 6))) %>%
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
    }
    if(str_to_lower(unit_id) == "admin1"){
      data <-
        data %>%
        filter(event_type %in% filter_types) %>%
        mutate(event_date = ymd(.data[[time_id]]),
               event_time = floor_date(.data$event_date, time_target,week_start = getOption('lubridate.week.start', 6))) %>%
        filter(between(.data$event_time, as.Date(start_date), as.Date(end_date))) %>%

        group_by(.data$country,.data$admin1, .data$event_time, event_type) %>%

        summarise(count = n()) %>%
        ungroup() %>%
        full_join(merge(add_unit_ids, all_dates) %>%
                    as_tibble() %>%
                    separate(x,c("country","admin1"),sep=";") %>%
                    rename(event_time =.data$y))%>%
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
        arrange(!!paste0("event_", time_target)) %>%
        suppressMessages() %>%
        suppressWarnings()
    }
    if(str_to_lower(unit_id) == "admin2"){
      data <-
        data %>%
        filter(event_type %in% filter_types) %>%
        mutate(event_date = ymd(.data[[time_id]]),
               event_time = floor_date(.data$event_date, time_target,week_start = getOption('lubridate.week.start', 6))) %>%
        filter(between(.data$event_time, as.Date(start_date), as.Date(end_date))) %>%

        group_by(.data$country,.data$admin1, .data$admin2, .data$event_time, event_type) %>%

        summarise(count = n()) %>%
        ungroup() %>%
        full_join(merge(add_unit_ids, all_dates) %>%
                    as_tibble() %>%
                    separate(x,c("country","admin1","admin2"),sep=";") %>%
                    rename(event_time =.data$y))%>%
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
        arrange(!!paste0("event_", time_target)) %>%
        suppressMessages() %>%
        suppressWarnings()
    }
    if(str_to_lower(unit_id) == "admin3"){
      data <-
        data %>%
        filter(event_type %in% filter_types) %>%
        mutate(event_date = ymd(.data[[time_id]]),
               event_time = floor_date(.data$event_date, time_target,week_start = getOption('lubridate.week.start', 6))) %>%
        filter(between(.data$event_time, as.Date(start_date), as.Date(end_date))) %>%

        group_by(.data$country,.data$admin1, .data$admin2,.data$admin3, .data$event_time, event_type) %>%

        summarise(count = n()) %>%
        ungroup() %>%
        full_join(merge(add_unit_ids, all_dates) %>%
                    as_tibble() %>%
                    separate(x,c("country","admin1","admin2","admin3"),sep=";") %>%
                    rename(event_time =.data$y))%>%
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
        arrange(!!paste0("event_", time_target)) %>%
        suppressMessages() %>%
        suppressWarnings()
    }
    if(str_to_lower(unit_id) == "location"){
      data <-
        data %>%
        filter(event_type %in% filter_types) %>%
        mutate(event_date = ymd(.data[[time_id]]),
               event_time = floor_date(.data$event_date, time_target,week_start = getOption('lubridate.week.start', 6))) %>%
        filter(between(.data$event_time, as.Date(start_date), as.Date(end_date))) %>%

        group_by(.data$country,.data$admin1, .data$admin2,.data$admin3,.data$location, .data$event_time, event_type) %>%

        summarise(count = n()) %>%
        ungroup() %>%
        full_join(merge(add_unit_ids, all_dates) %>%
                    as_tibble() %>%
                    separate(x,c("country","admin1","admin2","admin3","location"),sep=";") %>%
                    rename(event_time =.data$y))%>%
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
        arrange(!!paste0("event_", time_target)) %>%
        suppressMessages() %>%
        suppressWarnings()
    }
    }

    if(length(filter_types) == 1)
      data <- data %>% select(-.data$total_events) %>%
        suppressWarnings()

    return(data)

  }

