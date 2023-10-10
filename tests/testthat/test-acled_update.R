

#Proper functioning of the function ----

## There are no duplicates in the returned dataset ----

test_that("There are no duplicates in the returned data",{
  expect_equal(anyDuplicated(dupes_checks$event_id_cnty), 0L)
})

## Additional additional_countries are properly added to the dataset

test_that("Additional additional_countries are properly included",{
  expect_equal(
    append(unique(dupes_checks$country), c("Mexico", "Brazil")), unique(dupes_checks_plus_bramex$country))
})

## When disabling deleted events, users get a different result ----

test_that("acled_update without deletions actually returns events that should be deleted",{

  snap_test <- acled_update(acledR::acled_old_deletion_dummy,
                            email = "acledexamples@gmail.com",
                            key = "M3PWwg3DIdhHMuDiilp5", deleted = FALSE,
                            acled_access = F, prompts = F)

   expect_true(any(find_deleted_events$event_id_cnty %in% snap_test$event_id_cnty))
})

# Errors and messages ----
## An warning appears when requesting an update of data for dates earlier/later to the min of my dataset ----

test_that("Warning for earlier dates requested", {
  expect_warning(acled_update(acledR::acled_old_dummy,
                              additional_countries = "Argentina",
                              start_date = (min(acledR::acled_old_dummy$event_date) + 10),
                              email = "acledexamples@gmail.com",
                              key = "M3PWwg3DIdhHMuDiilp5",
                              acled_access = F, prompts = F),
                              regexp = "Start date is later")
})

test_that("Warning for start dates that are later than the earliest in the dataset", {
  expect_warning(acled_update(acledR::acled_old_dummy,
                              additional_countries = "Argentina",
                              start_date = (min(acledR::acled_old_dummy$event_date) - 10),
                              email = "acledexamples@gmail.com",
                              key = "M3PWwg3DIdhHMuDiilp5",
                              acled_access = F, prompts = F),
                 regexp = "Start date is earlier than")
})

## A warning appears when requesting an update of data for dates older/earlier to the max of my dataset ----

test_that("Warning for later end dates than requested", {
  expect_warning(acled_update(acledR::acled_old_dummy,
                              additional_countries = "Argentina",
                              end_date = (max(acledR::acled_old_dummy$event_date) + 10),
                              email = "acledexamples@gmail.com",
                              key = "M3PWwg3DIdhHMuDiilp5",
                              acled_access = F, prompts = F),
                 regexp = "End date is later than")
})

test_that("Warning for ealier end dates than the max requested", {
  expect_warning(acled_update(acledR::acled_old_dummy,
                              additional_countries = "Argentina",
                              end_date = (max(acledR::acled_old_dummy$event_date) - 10),
                              email = "acledexamples@gmail.com",
                              key = "M3PWwg3DIdhHMuDiilp5",
                              acled_access = F, prompts = F),
                 regexp = "End date is earlier than")
})



## Errors when additional_countries provided are not in acled_countries----

test_that("Error if `additional_countries` or `regions` are not in the dataset", {

  expect_error(acled_update(acledR::acled_old_dummy, additional_countries = "Unknown Country"),
               "Error: The following additional_countries are not present in acledR::acled_countries: Unknown Country")
  expect_error(acled_update(acledR::acled_old_dummy, regions = "Unknown Region"),
               "Error: The following regions are not present in acledR::acled_regions: Unknown Region")

})


## Errors if acled_access is used incorrectly ----

test_that("An error appears if acled_access is false but no keys are provided",{
  expect_error(acled_update(acledR::acled_old_dummy,
                            additional_countries = "Argentina",
                            start_date = min(acled_old_dummy$event_date),
                            acled_access = F, prompts = F), regexp = "Error: If acled_access is FALSE")
})

## Errors when the dataset does not have the acled structure ----

test_that("If you have a dataset that does not match acled's structure you get an error",{
  df <- data.frame(
    x="a", y="b", c="c"
  )
  expect_error(acled_update(df,
                            additional_countries = "Argentina",
                            start_date = "2022-01-01",
                            acled_access = F, prompts = F), regexp = "The data frame provided does not have ACLED's structure")
})

## Errors when requesting event types that are not part of ACLED's event types ----

test_that("Users get an error when requesting a non existent event type",{
  expect_error(acled_update(acledR::acled_old_dummy,
                               email = "acledexamples@gmail.com",
                               key = "M3PWwg3DIdhHMuDiilp5",
                              event_types = "Snowball fights",
                               acled_access = F, prompts = F), regexp = "Error: Invalid event_type provided")
  })
# Weird use cases ----

## Does not generate duplicates when fed a non-unique list of values. - But it takes a long time ----

test_that("No duplicates when the function is given a non-uniques list of values in the additional_countries argument.", {
  expect_equal(anyDuplicated(test_more_than_one), 0L)
})

