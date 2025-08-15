
library(acledR)


# received_data <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"),
#                            key = Sys.getenv("EXAMPLES_KEY"),
#                            country="Nigeria",
#                            start_date="2023-01-01",
#                            end_date = "2024-02-01",
#                            prompt = F,
#                            inter_numeric = TRUE)



received_data_new <- acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                               password = Sys.getenv("ACLED_API_PASSWORD"),
                               country="Nigeria",
                               start_date="2023-01-01",
                               end_date = "2024-02-01",
                               inter_numeric = TRUE)


# dim(received_data)
# dim(received_data_new)

### BAD ###
# received_data_monadic <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
#                                    country = "Argentina", start_date="2022-01-01",end_date = "2022-02-01",
#                                    prompt = F, monadic = T, acled_access = F, log = F, inter_numeric = TRUE)

received_data_monadic_new <- acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                       password = Sys.getenv("ACLED_API_PASSWORD"),
                                       country="Argentina",
                                       start_date="2022-01-01",
                                       end_date = "2022-02-01",
                                       monadic = TRUE,
                                       inter_numeric = TRUE)





### GOOD ###
# log_received_data <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
#                                regions = c("Western Africa", "Eastern Africa", "Europe"),
#                                start_date="2022-01-01",end_date = "2022-12-31",
#                                prompt = FALSE, acled_access = FALSE,
#                                log = TRUE, inter_numeric = TRUE)

log_received_data_new <- acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                   password = Sys.getenv("ACLED_API_PASSWORD"),
                                   regions = c("Western Africa", "Eastern Africa", "Europe"),
                                   start_date="2022-01-01",end_date = "2022-12-31",
                                   log = TRUE,
                                   inter_numeric = TRUE)


### GOOD ###
# received_data_numeric_region <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
#                                           regions = 1, prompt = F, acled_access = F, inter_numeric = TRUE)

received_data_numeric_region_new <- acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                              password = Sys.getenv("ACLED_API_PASSWORD"),
                                              regions = 7,
                                              inter_numeric = TRUE)


### GOOD ###
# timestamp_numeric_check <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
#                                      country = "Argentina",
#                                      start_date="2023-01-01",
#                                      end_date = "2025-06-06",
#                                      timestamp = 1673295342, # as numeric
#                                      prompt = F, acled_access = F, log = F, inter_numeric = TRUE)

timestamp_numeric_check_new <- acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                         password = Sys.getenv("ACLED_API_PASSWORD"),
                                         country = "Argentina",
                                         start_date="2023-01-01",
                                         end_date = "2025-06-06",
                                         timestamp = 1673295342, # as numeric
                                         inter_numeric = TRUE)




### UNSURE ###
# timestamp_string_check <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
#                                     country = "Argentina",
#                                     start_date="2023-01-01",
#                                     end_date = "2023-06-06",
#                                     timestamp = "2023-04-16", # as numeric
#                                     prompt = F, acled_access = F, log = F, inter_numeric = TRUE)

timestamp_string_check_new <- acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                        password = Sys.getenv("ACLED_API_PASSWORD"),
                                        country = "Argentina",
                                        start_date="2023-01-01",
                                        end_date = "2023-06-06",
                                        timestamp = "2023-04-16", # as numeric
                                        # prompt = F, acled_access = F, log = F,
                                        inter_numeric = TRUE)


# For checking credentials
# log_received_data_check_credential <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
#                                                 regions = c("Western Africa", "Eastern Africa", "Europe"),end_date = "2022-12-31",prompt = F, acled_access = T, log = T, inter_numeric = TRUE)


columns <- c("event_id_cnty","event_date","year","time_precision","disorder_type",
             "event_type","sub_event_type","actor1","assoc_actor_1","inter1","actor2","assoc_actor_2",
             "inter2","interaction","civilian_targeting","iso","region","country","admin1","admin2","admin3","location","latitude",
             "longitude","geo_precision","source","source_scale","notes","fatalities","tags","timestamp")


### GOOD ###
# received_data_country_and_region <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
#                                               country = "Argentina",
#                                               regions = "Central America",
#                                               start_date="2022-01-01",
#                                               end_date = "2022-02-01",
#                                               prompt = F, acled_access = F, inter_numeric = TRUE)


received_data_country_and_region_new <- acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                                  password = Sys.getenv("ACLED_API_PASSWORD"),
                                                  country = "Argentina",
                                                  regions = "Central America",
                                                  start_date="2022-01-01",
                                                  end_date = "2022-02-01",
                                                  inter_numeric = TRUE)



### GOOD ###
# received_data_country_and_region_num <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
#                                                   country = "Argentina",
#                                                   regions = 14,
#                                                   start_date="2022-01-01",
#                                                   end_date = "2022-02-01",
#                                                   prompt = F, acled_access = F, inter_numeric = TRUE)


received_data_country_and_region_num_new <- acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                                      password = Sys.getenv("ACLED_API_PASSWORD"),
                                                      country = "Argentina",
                                                      regions = 14,
                                                      start_date="2022-01-01",
                                                      end_date = "2022-02-01",
                                                      inter_numeric = TRUE)
