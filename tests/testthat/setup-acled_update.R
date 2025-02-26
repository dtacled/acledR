
if(identical(Sys.getenv("NOT_CRAN"), "true")) {

  dupes_checks <- acled_update(acledR::acled_old_dummy,
                               email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                               acled_access = F, prompts = F,
                               inter_numeric = TRUE)

  dupes_checks_plus_bramex <- acled_update(acledR::acled_old_dummy,
                                           additional_countries = c("Brazil","Mexico"),
                                           email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                           acled_access = F, prompts = F,
                                           inter_numeric = TRUE)


  test_more_than_one <- acled_update(acledR::acled_old_deletion_dummy,
                                     email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                     acled_access = F, prompts = F,
                                     inter_numeric = TRUE)

  find_deleted_events <- acled_deletions_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                             date_deleted = max(acledR::acled_old_deletion_dummy$timestamp),
                                             acled_access = F)

}
