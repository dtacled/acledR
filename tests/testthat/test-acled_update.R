

#Proper functioning of the function ----

## There are no duplicates in the returned dataset ----

test_that("There are no duplicates in the returned data",{
  expect_equal(anyDuplicated(dupes_checks$event_id_cnty), 0L)
})


## Missing all bucket 1 unit tests (basic tests of output and proper functioning.)


# Errors and messages ----
## An warning appears when requesting an update of data for dates earlier to the min of my dataset ----

test_that("Warning for earlier dates requested", {
  expect_warning(acled_update(acledR::acled_old_dummy,
                              countries = "Argentina",
                              start_date = (min(acledR::acled_old_deletion_dummy$event_date) - 10),
                              email = "acledexamples@gmail.com",
                              key = "M3PWwg3DIdhHMuDiilp5",
                              acled_access = F, prompts = F),
                              regexp = "Start date is earlier than")
})

## An warning appears when requesting an update of data for dates older to the max of my dataset ----

test_that("Warning for later dates requested", {
  expect_warning(acled_update(acledR::acled_old_dummy,
                              countries = "Argentina",
                              end_date = (max(acledR::acled_old_deletion_dummy$event_date) + 10),
                              email = "acledexamples@gmail.com",
                              key = "M3PWwg3DIdhHMuDiilp5",
                              acled_access = F, prompts = F),
                 regexp = "End date is later than")
})


## Errors when countries provided are not in acled_countries----

test_that("Error if `countries` or `regions` are not in the dataset", {

  expect_error(acled_update(acledR::acled_old_dummy, countries = "Unknown Country"),
               "Error: The following countries are not present in acledR::acled_countries: Unknown Country")
  expect_error(acled_update(acledR::acled_old_dummy, regions = "Unknown Region"),
               "Error: The following regions are not present in acledR::acled_regions: Unknown Region")

})


# Weird use cases ----

## Does not generate duplicates when fed a non-unique list of values. - But it takes a long time ----

test_that("No duplicates when the function is given a non-uniques list of values in the countries argument.", {
  expect_equal(anyDuplicated(test_more_than_one), 0L)
})

