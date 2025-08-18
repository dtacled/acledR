# Helpers for test-acled_api.R


if(identical(Sys.getenv("NOT_CRAN"), "true")) {

  received_deleted_data_date_new <- acled_deletions_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                                    password = Sys.getenv("ACLED_API_PASSWORD"),
                                                    date_deleted = "2022-07-25", acled_access = F)
  received_deleted_data_unix_new <- acled_deletions_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                                    password = Sys.getenv("ACLED_API_PASSWORD"),
                                                    date_deleted = "1658707200", acled_access = F)


  received_deleted_log_new <- acled_deletions_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                              password = Sys.getenv("ACLED_API_PASSWORD"),
                                              date_deleted = "1658707200", acled_access = F, log = T)

  columns_deleted <- c("event_id_cnty", "timestamp")
}
