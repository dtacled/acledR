# Helpers for test-acled_api.R


if(identical(Sys.getenv("NOT_CRAN"), "true")) {

  received_deleted_data_date <- acled_deletions_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                                    password = Sys.getenv("ACLED_API_PASSWORD"),
                                                    date_deleted = "2022-07-25")

  received_deleted_data_unix <- acled_deletions_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                                    password = Sys.getenv("ACLED_API_PASSWORD"),
                                                    date_deleted = "1658707200")


  received_deleted_log <- acled_deletions_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                              password = Sys.getenv("ACLED_API_PASSWORD"),
                                              date_deleted = "1658721600")

  columns_deleted <- c("event_id_cnty", "deleted_timestamp")
}


as.numeric(as.POSIXct("2022-07-25", format="%Y-%m-%d"))

