# library(tidyverse)
# library(acledR)
# library(testthat)


data <- acled_api(email = "acledexamples@gmail.com",
                  key = "M3PWwg3DIdhHMuDiilp5",
                  countries = "Georgia", ... = "&year=2022",
                  acled_access = FALSE, prompt = FALSE, log = FALSE)

data2 <- acled_api(email = "acledexamples@gmail.com",
                  key = "M3PWwg3DIdhHMuDiilp5",
                  countries = c("Georgia","Finland"), ... = "&year=2022",
                  acled_access = FALSE, prompt = FALSE, log = FALSE)


data3 <- acled_api(email = "acledexamples@gmail.com",
                   key = "M3PWwg3DIdhHMuDiilp5",
                   countries = c("Argentina","Spain"), ... = "&year=2022",
                   acled_access = FALSE, prompt = FALSE, log = FALSE)


do_we_match <- acled_generate_counts(data3, event_type = "Protests", unit_id = "admin1", time_target = "month", time_id = "event_date")



data_count_month <- data %>%
  group_by(month = floor_date(event_date, unit = "month")) %>%
  summarise(n = n())

weekly_protests_tbilisi <- acled_generate_counts(data,
                                                event_type = c("Protests"),
                                                unit_id = "admin1",
                                                time_id = "event_date",
                                                time_target = "week") %>%
  filter(admin1 == "Tbilisi")
weekly_riots_tbilisi <- acled_generate_counts(data,
                                                event_type = c("Riots"),
                                                unit_id = "admin1",
                                                time_id = "event_date",
                                                time_target = "week") %>%
  filter(admin1 == "Tbilisi")

non_count_week_tbilisi <- data %>%
  group_by(week = floor_date(event_date, "week",
                             week_start = getOption("lubridate.week.start", 6)
                            ),
           admin1, event_type) %>%
  summarise(n = n()) %>%
  filter(event_type == "Protests", admin1 == "Tbilisi") %>%
  select(admin1, event_week = week, protests = n)


# ### Debugging
#
# time_target <- "week"
# start_date <- floor_date(ymd(min(data3$event_date)), unit = time_target,week_start = getOption('lubridate.week.start', 6))
# end_date <- floor_date(ymd(max(data3$event_date)), unit = time_target,week_start = getOption('lubridate.week.start', 6))
# time_id <- "event_date"
#
#
# all_dates <- seq(floor_date(as.Date(start_date), time_target,
#                             week_start = getOption('lubridate.week.start', 6)),
#                  floor_date(as.Date(end_date), time_target,
#                             week_start = getOption('lubridate.week.start', 6)),
#                  by = time_target)
#
#
#
#
# # For admin1 - merging
#
# unit_id <- "admin1"
# filter_types <- unique(data3$event_type)
# to_add <- "The fairy of fairies"
#
# add_unit_ids <- data3 %>%
#   select(.data$country,.data$admin1)%>%
#   unite(for_merging,country,admin1, sep=";")%>%
#   distinct() %>%
#   as.list()%>%
#   .$for_merging
#
# add_unit_ids <- unique(c(add_unit_ids, to_add))
#
# making_a_check<-
#   data3 %>%
#     filter(event_type %in% filter_types) %>%
#     mutate(event_date = ymd(.data[[time_id]]),
#            event_time = floor_date(.data$event_date, time_target,week_start = getOption('lubridate.week.start', 6))) %>%
#     filter(between(.data$event_time, as.Date(start_date), as.Date(end_date))) %>%
#
#     group_by(.data$country,.data$admin1, .data$event_time, event_type) %>%
#
#     summarise(fatalities = sum(fatalities)) %>%
#     ungroup() %>%
#     full_join(merge(add_unit_ids, all_dates) %>%
#                 as_tibble() %>%
#                 separate(x,c("country","admin1"),sep=";") %>%
#                 rename(event_time =.data$y))%>%
#   mutate(fatalities = case_when(is.na(fatalities) ~ as.numeric(0),
#                            TRUE ~ as.numeric(fatalities))) %>%
#   rename(!!paste0("event_", time_target) := .data$event_time) %>%
#   pivot_wider(values_from = fatalities, names_from = event_type) %>%
#   mutate(across(.cols = where(is.numeric), ~case_when(is.na(.) ~ 0, TRUE ~ .))) %>%
#   rowwise() %>%
#   mutate(total_fatalities = sum(c_across(where(is.numeric)))) %>%
#   janitor::clean_names() %>%
#   ungroup() %>%
#   select(-one_of("na")) %>%
#   arrange(!!paste0("event_", time_target)) %>%
#   suppressMessages() %>%
#   suppressWarnings()
#
# ## Lets check for accuracy
#
# checking <- acled_generate_counts(data3, event_type = "Protests", unit_id = "admin1", time_id = "event_date", time_target = "week")%>%
#   filter(admin1 == "La Rioja")
#
# checking_fatalities <- acled_generate_counts(data3, unit_id = "admin1", time_id = "event_date", time_target = "week", fatalities_count = T)
#
# view(
#   data3 %>%
#     #group_by(country,admin1, week = floor_date(event_date, "week",
#      #                                                 week_start = getOption("lubridate.week.start", 6))) %>%
#     mutate(week = floor_date(event_date, "week",week_start = getOption("lubridate.week.start", 6)))%>%
#     relocate(week,.before=event_date)%>%
#     filter(fatalities > 0)
# )
#
# sum(data3$fatalities)
