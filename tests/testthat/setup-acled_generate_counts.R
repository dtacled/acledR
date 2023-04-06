# library(tidyverse)
# library(acledR)
# library(testthat)
#

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
# start_date <- floor_date(min(data3$event_date), "month", week_start = getOption('lubridate.week.start', 6))
# end_date <- floor_date(max(data3$event_date), "month", week_start = getOption('lubridate.week.start', 6))
# time_id <- "event_date"
#
#
# #For admin2
# add_unit_ids <- data3 %>%
#   select(.data$country,.data$admin1,.data$admin2,.data$admin3,.data$location)%>%
#   unite(for_merging,country,admin1,admin2,admin3,location, sep=";")%>%
#   distinct() %>%
#   as.list()
#
# all_dates <- seq(floor_date(as.Date(start_date), time_target,
#                             week_start = getOption('lubridate.week.start', 6)),
#                  floor_date(as.Date(end_date), time_target,
#                             week_start = getOption('lubridate.week.start', 6)),
#                  by = time_target)
#
# merge(add_unit_ids$for_merging, all_dates) %>%
#             as_tibble() %>%
#             separate(x,c("country","admin1","admin2", "admin3", "location"),sep=";") %>%
#             rename(event_time =.data$y)
#
#
# # For admin1 - merging
#
# unit_id <- "admin1"
# filter_types <- "Protests"
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
# view(
#   data3 %>%
#     filter(event_type %in% filter_types) %>%
#     mutate(event_date = ymd(.data[[time_id]]),
#            event_time = floor_date(.data$event_date, time_target,week_start = getOption('lubridate.week.start', 6))) %>%
#     filter(between(.data$event_time, as.Date(start_date), as.Date(end_date))) %>%
#
#     group_by(.data$country,.data$admin1, .data$event_time, event_type) %>%
#
#     summarise(count = n()) %>%
#     ungroup() %>%
#     full_join(merge(add_unit_ids, all_dates) %>%
#                 as_tibble() %>%
#                 separate(x,c("country","admin1"),sep=";") %>%
#                 rename(event_time =.data$y))%>%
#   mutate(count = case_when(is.na(count) ~ as.numeric(0),
#                            TRUE ~ as.numeric(count))) %>%
#   rename(!!paste0("event_", time_target) := .data$event_time) %>%
#   pivot_wider(values_from = count, names_from = event_type) %>%
#   mutate(across(.cols = where(is.numeric), ~case_when(is.na(.) ~ 0, TRUE ~ .))) %>%
#   rowwise() %>%
#   mutate(total_events = sum(c_across(where(is.numeric)))) %>%
#   janitor::clean_names() %>%
#   ungroup() %>%
#   select(-one_of("na")) %>%
#   arrange(!!paste0("event_", time_target)) %>%
#   suppressMessages() %>%
#   suppressWarnings()
# )
# ## Lets check for accuracy
#
# checking <- acled_generate_counts(data3, event_type = "Protests", unit_id = "admin1", time_id = "event_date", time_target = "week")%>%
#   filter(admin1 == "La Rioja")
#
#
#
# view(
#   data3 %>%
#     filter(event_type == "Protests" & str_detect(admin1,"La Rioja")) %>%
#     group_by(country,admin1,admin2, week = floor_date(event_date, "week",
#                                                       week_start = getOption("lubridate.week.start", 6))) %>%
#       summarise(n=n())
# )
