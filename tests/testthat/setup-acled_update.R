
if(identical(Sys.getenv("NOT_CRAN"), "true")) {

  dupes_checks <- acled_update(acledR::acled_old_dummy,
                               email = Sys.getenv("ACLED_API_EMAIL"),
                               password = Sys.getenv("ACLED_API_PASSWORD"),
                               inter_numeric = TRUE)

  dupes_checks_plus_bramex <- acled_update(acledR::acled_old_dummy,
                                           additional_countries = c("Brazil","Mexico"),
                                           email = Sys.getenv("ACLED_API_EMAIL"),
                                           password = Sys.getenv("ACLED_API_PASSWORD"),
                                           inter_numeric = TRUE)


  test_more_than_one <- acled_update(acledR::acled_old_deletion_dummy,
                                     email = Sys.getenv("ACLED_API_EMAIL"),
                                     password = Sys.getenv("ACLED_API_PASSWORD"),
                                     inter_numeric = TRUE)

  find_deleted_events <- acled_deletions_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                             password = Sys.getenv("ACLED_API_PASSWORD"),
                                             date_deleted = max(acledR::acled_old_deletion_dummy$timestamp))

}
