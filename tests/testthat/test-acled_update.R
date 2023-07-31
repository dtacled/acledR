

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

# Weird use cases ----

## Does not generate duplicates when fed a non-unique list of values. - But it takes a long time ----

test_that("No duplicates when the function is given a non-uniques list of values in the additional_countries argument.", {
  expect_equal(anyDuplicated(test_more_than_one), 0L)
})

