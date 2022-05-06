library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)


df <-
  df_api %>%
  filter(country %in% c("Nigeria", "Guam"))



temp <-
  generate_counts(df,
                  # event_type = c('Battles', "Riots"),
                  unit_id = "country",
                  time_id = "event_date",
                  time_target = "month",
                  start_date = NULL,
                  end_date = NULL,
                  add_unit_ids = c("Fiji", "asdf"))

temp$country %>% table()


temp %>%
  ggplot() +
  geom_line(aes(x = event_month, y = count, color = event_type)) +
  facet_wrap(~country)


temp %>%
  arrange(count) %>% View()

  generate_movers(., "country",
                  "event_month",
                  var = "count",
                  slide_funs = c("mean", "sd"),
                  slide_periods = c(1, 12), na.rm = T,
                  complete = T)



temp %>%
  ggplot() +
  geom_line(aes(x = event_month, y = count, group = country))


