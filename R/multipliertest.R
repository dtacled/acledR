# # The call
#
# library(lubridate)
# library(tidyverse)
# library(acledR)
#
# country <- c("Argentina", "Brazil")
# start_date <- ymd("2020-01-01")
# end_date <- ymd("2022-12-31")
#
#
#
# middle_point <- paste0(year(end_date) - 1, "-12-31")
#
# n_days_dates <- as.numeric((ymd(end_date) - ymd(start_date)) / 365)
#
#
# ymd(start_date) + 365
#
#
# estimate <- function(countries,start_date, end_date) {
#
#   multipliers <- acledR::acled_multipliers %>%
#     rename(country1 = country) %>%
#     filter(country1 %in% country)
#
#   start_date <- as.Date(start_date)
#   end_date <- as.Date(end_date)
#
#   # Create a data frame with a sequence of dates from start_date to end_date
#   date_seq <- data.frame(date = seq.Date(start_date, end_date, by = "day")) %>%
#     mutate(year = format(date, "%Y")) %>%
#     group_by(year) %>%
#     summarise(days_in_year = n()) %>%
#     mutate(year = as.integer(year))
#
#   joined_multipliers <- date_seq %>%
#     left_join(multipliers, by = "year") %>%
#     filter(!is.na(avg_month_bin)) %>%
#     mutate(estimated_events = ((avg_month_bin/30) * days_in_year))
#
#
#   return(joined_multipliers)
# }
#
# ee <- estimate(country,start_date,end_date)
#
#
# received_data <- acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",countries = c("Argentina","Brazil"), start_date="2020-01-01",end_date = "2022-12-31",prompt = F, acled_access = T, log = F)
#
# received_data %>%
#   group_by(country, year) %>%
#   summarise(n = n())
#
