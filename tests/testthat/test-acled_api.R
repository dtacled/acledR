# acled_api unit testing

# Basic functioning ----
test_that("number of columns is correct", {
  expect_equal(ncol(received_data),31)
})

test_that("names of columns are correct", {
  expect_equal(names(received_data),columns)
})

test_that("Split calls for big calls", {
  expect_equal(as.numeric(ceiling(sum(log_received_data$time)/300000)),max(log_received_data$calls))
})

# Regions are managed properly
test_that("Regions in numeric work",{
  expect_true(all.equal(data.frame(region="Western Africa",rows=1:nrow(received_data_numeric_region))$region,received_data_numeric_region$region))})

#   Test what happens when someone requests a region and a country of another region

test_that("Testing that when requestion a region, and a country of another region, you get both",{

  list_countries <- acledR::acled_countries %>%
    filter(region == "Central America") %>%
    unique(x=.$country) %>%
    append("Argentina")


  expect_setequal(unique(received_data_country_and_region$country), list_countries)

})


# Errors ----
# Error when someone requests a region that does not exist

test_that("Error prompted when region does not exist", {
  expect_error(acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",regions = "Narnia",
                         start_date="2022-01-01",end_date = "2022-12-31",prompt = F, acled_access = F, log = F), regexp = "One or more requested region names not in the ACLED country list.")
})

test_that("Error when region number does not exist", {
  expect_error(acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",regions = 420,
                         start_date="2022-01-01",end_date = "2022-12-31",prompt = F, acled_access = F, log = F),
               regexp = "One or more requested region numbers not in the ACLED country list")
})

# Errors when a country requested doesnt exists
test_that("Error when one of two countries are wrong",{
          expect_error(acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",countries = c("Argentia","Bolivia"),
                                 start_date="2022-01-01",end_date = "2022-12-31",prompt = F, acled_access = F, log = F),
                       regexp = "One or more of the requested countries *")})

# Test what happens when someone inputs acled_access as TRUE but it includes email and key.
test_that("Acled_access is ignored",{
  expect_true(grepl("acledexamples", log_received_data_check_credential$email[1]))
})

# Test errors from incorrectly input arguments.

test_that("acled_api() throws an error when called with invalid arguments", {
  expect_error(acled_api(Countries = "Argentina",
                         start_date="2022-01-01",
                         end_date = "2022-12-31",
                         prompt = F,
                         acled_access = T,
                         log = F), regexp=
               "Countries is not a valid option. Please utilize \"countries\", without capitalizing")

  expect_error(acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",
                         Region = "North America",
                         start_date="2022-01-01",
                         end_date = "2022-12-31",
                         prompt = F,
                         acled_access = T,
                         log = F), regexp=
               "Region is not a valid option. Please utilize \"regions\"")

  expect_error(acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",
                         Regions = "North America",
                         start_date="2022-01-01",
                         end_date = "2022-12-31",
                         prompt = F,
                         acled_access = T,
                         log = F), regexp=
               "Regions is not a valid option. Please utilize \"regions\", without capitalizing")

  expect_error(acled_api(Event_type = "Argentina",
                         start_date="2022-01-01",
                         end_date = "2022-12-31",
                         prompt = F,
                         acled_access = T), regexp=
               "Event type is not a valid option. Please utilize \"event_types\", without capitalizing")
  expect_error(acled_api(countries = "Argentina",
                         Start_date="2022-01-01",
                         end_date = "2022-12-31",
                         prompt = F,
                         acled_access = T), regexp=
               "Start_date is not a valid option. Please utilize \"start_date\", without capitalizing")
  expect_error(acled_api(countries = "Argentina",
                         start_date="2022-01-01",
                         End_date = "2022-12-31",
                         prompt = F,
                         acled_access = T), regexp=
               "End_date is not a valid option. Please utilize \"end_date\", without capitalizing")
})

# Test errors from badly utilized acled_access and key/email combination
test_that("If access is TRUE and credentials are null, credentials are ignored, but an error appears if Keys are empty in the enviornemt", {
  expect_error(
    Sys.setenv("acled_key" = "") %>%
      acled_api(Country = "Argentina",
                start_date="2022-01-01",
                end_date = "2022-12-31",
                prompt = F,
                acled_access = T,
                log = F), regexp = "acled_access is TRUE, but email and/or key are not stored in the enviornment. Please rerun acled_access or include key and email in function")
})

