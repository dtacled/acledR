# Helpers for test-acled_api.R

if(identical(Sys.getenv("NOT_CRAN"), "true")) {

  received_deleted_data_date <- acled_deletions_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"), date_deleted = "2022-07-25", acled_access = F)
  received_deleted_data_unix <- acled_deletions_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"), date_deleted = "1658707200", acled_access = F)


  received_deleted_log <- acled_deletions_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"), date_deleted = "1658707200", acled_access = F, log = T)

  columns_deleted <- c("event_id_cnty", "deleted_timestamp")
}
