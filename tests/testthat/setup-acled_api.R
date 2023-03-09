# Helpers for test-acled_api.R

received_data <- acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",countries = "Argentina", start_date="2022-01-01",end_date = "2022-12-31",prompt = F, acled_access = F, log = F)
log_received_data <- acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",regions = c("Western Africa", "Eastern Africa", "Europe"),end_date = "2022-12-31",prompt = F, acled_access = F, log = T)
received_data_numeric_region <- acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",regions = 1,end_date = "2022-12-31",prompt = F, acled_access = F)

is_empty <- acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5", start_date='2021-01-01', acled_access = F,prompt=F)


columns <- c("data_id","iso","event_id_cnty","event_id_no_cnty","event_date","year","time_precision",
             "event_type","sub_event_type","actor1","assoc_actor_1","inter1","actor2","assoc_actor_2",
             "inter2","interaction","region","country","admin1","admin2","admin3","location","latitude",
             "longitude","geo_precision","source","source_scale","notes","fatalities","timestamp","iso3")
