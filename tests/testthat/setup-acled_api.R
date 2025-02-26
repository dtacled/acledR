# Helpers for test-acled_api.R

if(identical(Sys.getenv("NOT_CRAN"), "true")) {

  received_data <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),country="Argentina", start_date="2022-01-01", end_date = "2022-12-31", prompt = F, acled_access = T, log = F, inter_numeric = TRUE)
  received_data_monadic <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),country = "Argentina", start_date="2022-01-01",end_date = "2022-12-31",prompt = F, monadic = T, acled_access = F, log = F, inter_numeric = TRUE)
  log_received_data <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),regions = c("Western Africa", "Eastern Africa", "Europe"), start_date="2022-01-01",end_date = "2022-12-31",prompt = F, acled_access = F, log = T, inter_numeric = TRUE)
  received_data_numeric_region <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),regions = 1,prompt = F, acled_access = F, inter_numeric = TRUE)

  timestamp_numeric_check <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                       country = "Argentina",
                                       start_date="2023-01-01",end_date = "2023-06-06",
                                       timestamp = 1681622333, # as numeric
                                       prompt = F, acled_access = F, log = F, inter_numeric = TRUE)

  timestamp_string_check <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                      country = "Argentina",
                                      start_date="2023-01-01",end_date = "2023-06-06",
                                      timestamp = "2023-04-16", # as numeric
                                      prompt = F, acled_access = F, log = F, inter_numeric = TRUE)

  # For checking credentials
  log_received_data_check_credential <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                                  regions = c("Western Africa", "Eastern Africa", "Europe"),end_date = "2022-12-31",prompt = F, acled_access = T, log = T, inter_numeric = TRUE)


  columns <- c("event_id_cnty","event_date","year","time_precision","disorder_type",
               "event_type","sub_event_type","actor1","assoc_actor_1","inter1","actor2","assoc_actor_2",
               "inter2","interaction","civilian_targeting","iso","region","country","admin1","admin2","admin3","location","latitude",
               "longitude","geo_precision","source","source_scale","notes","fatalities","tags","timestamp")

  received_data_country_and_region <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                                country = "Argentina",
                                                regions = "Central America",
                                                start_date="2022-01-01",
                                                end_date = "2022-12-31",
                                                prompt = F, acled_access = F, inter_numeric = TRUE)

  received_data_country_and_region_num <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                                    country = "Argentina",
                                                    regions = 14,
                                                    start_date="2022-01-01",
                                                    end_date = "2022-12-31",
                                                    prompt = F, acled_access = F, inter_numeric = TRUE)
}
