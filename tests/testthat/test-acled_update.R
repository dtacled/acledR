

#Proper functioning of the function

## There are no duplicates in the returned dataset

test_that("There are no duplicates in the returned data",{
  expect_equal(anyDuplicated(dupes_checks$event_id_cnty), 0L)
})



# Weird use cases

## What happends when someone provides a non-uniques list of countries to the countries argument?



# Errors and messages ----
## An warning appears when requesting an update of data for dates earlier to the min of my dataset

test_that("Warning for earlier dates requested", {
  expect_warning(acled_update(acledR::acled_old_dummy,
                              countries = "Argentina",
                              start_date = (min(acledR::acled_old_deletion_dummy$event_date) - 10),
                              email = "acledexamples@gmail.com",
                              key = "M3PWwg3DIdhHMuDiilp5",
                              acled_access = F, prompts = F),
                              regexp = "Start date is earlier than")
})

# An warning appears when requesting an update of data for dates older to the max of my dataset

test_that("Warning for later dates requested", {
  expect_warning(acled_update(acledR::acled_old_dummy,
                              countries = "Argentina",
                              end_date = (max(acledR::acled_old_deletion_dummy$event_date) + 10),
                              email = "acledexamples@gmail.com",
                              key = "M3PWwg3DIdhHMuDiilp5",
                              acled_access = F, prompts = F),
                 regexp = "End date is later than")
})


