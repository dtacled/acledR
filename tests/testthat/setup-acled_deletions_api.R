# Helpers for test-acled_api.R

received_deleted_data_date <- acled_deletions_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5", date_deleted = "2022-07-25", acled_access = F)
received_deleted_data_unix <- acled_deletions_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5", date_deleted = "1658707200", acled_access = F)


columns <- c("event_id_cnty", "deleted_timestamp")
