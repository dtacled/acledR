# acled_api unit testing



# Basic functioning ----
test_that("number of columns is correct", {
  skip_on_cran()
  expect_equal(ncol(received_data),31)
})

test_that("names of columns are correct", {
  skip_on_cran()
  expect_equal(names(received_data),columns)
})

## test if event_type filters work----
test_that("event_type filters work or not",{
  skip_on_cran()
  expect_equal(unique(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                         start_date="2022-01-01",end_date = "2022-12-31", country = "Argentina",
                         event_type = "Protests", prompt = F, acled_access = F, log = F, inter_numeric = TRUE)$event_type), "Protests" )

})

## Handling big calls ----
test_that("Split calls for big calls", {
  skip_on_cran()
  expect_equal(as.numeric(ceiling(sum(log_received_data$time)/300000)),max(log_received_data$calls))
})

test_that("country days are calculated as expected",{
  skip_on_cran()
  argentina_country_days <- acledR::acled_countries %>%
    filter(country == "Argentina") %>%
    mutate(t_end = lubridate::ymd("2021-01-01"),
           unit_test = t_end - ymd(paste0(start_year, "-01-01")))

  argentina_test_call <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                   country = "Argentina", start_date="1998-01-01",
                                   end_date = "2021-01-01",prompt = F, acled_access = F, log = T, inter_numeric = TRUE)

  expect_equal(argentina_test_call$time, argentina_country_days$unit_test)

})

local({
  skip_on_cran()

  local_mocked_bindings(menu = function(choices,title=NULL) 1)

  test_that("Users continue call", {

    expect_equal(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                           start_date="2022-01-01",end_date = "2022-12-31",country = "Argentina",
                           prompt = T, acled_access = F, log = F, inter_numeric = TRUE),
                 acled_api(Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                           start_date="2022-01-01",end_date = "2022-12-31",country = "Argentina",
                           prompt = F, acled_access = F, log = F, inter_numeric = TRUE))
  })
}) # test on whether they can continue

## Regions are managed properly ----
test_that("Regions in numeric work",{
  skip_on_cran()
  expect_true(all.equal(data.frame(region="Western Africa",rows=1:nrow(received_data_numeric_region))$region,received_data_numeric_region$region))})

## Test what happens when someone requests a region and a country of another region ----

test_that("Testing that when requestion a region, and a country of another region, you get both",{
  skip_on_cran()
  list_countries <- acledR::acled_countries %>%
    filter(region == "Central America") %>%
    unique(x=.$country) %>%
    append("Argentina")


  expect_setequal(unique(received_data_country_and_region$country), list_countries)

})

test_that("When requesting a region with a numeric input, and a country of another region, you get both",{
  skip_on_cran()
  list_countries <- acledR::acled_countries %>%
    filter(region == "Central America") %>%
    unique(x=.$country) %>%
    append("Argentina")

  expect_setequal(unique(received_data_country_and_region_num$country), list_countries)

})

## Timestamp works as required ----

test_that("timestamp (numeric) actually gets used as filter", {
  skip_on_cran()
  expect_gte(min(timestamp_numeric_check$timestamp), 1681622333)
})

test_that("timestamp (string) actually gets used as filter", {
  skip_on_cran()
  expect_gte(min(timestamp_string_check$timestamp), 1681588800)
})

## A menu is prompted when the user provides a non-recognized timestamp, allowing users to either stop or continue----

local({
  skip_on_cran()

  local_mocked_bindings(menu = function(choices,title=NULL) 2)

  test_that("A user can stop a call when the provided timestamp is not recognized",{

  expect_error(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                         start_date="2022-01-01",end_date = "2022-12-31",country = "Argentina",
                         timestamp = "muchachos", prompt = F, acled_access = F, log = F, inter_numeric = TRUE),
               regexp =  "User requested")
  })
})

local({
  skip_on_cran()
  local_mocked_bindings(menu = function(choices,title=NULL) 1)

  test_that("A user can ignore the provided timestamp if it is not recognized",{

    expect_no_error(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                           start_date="2022-01-01",end_date = "2022-12-31",country = "Argentina",
                           timestamp = "muchachos", prompt = F, acled_access = F, log = F, inter_numeric = TRUE))
  })
})


## When asking for monadics, it returns monadics ----

test_that("The call actually returns monadics.", {
  skip_on_cran()
  expect_equal(min(received_data_monadic$event_date), min(received_data$event_date))

  expect_equal(max(received_data_monadic$event_date), max(received_data$event_date))

  expect_equal(unique(received_data_monadic$country), unique(received_data$country))

  expect_gte(nrow(received_data_monadic), nrow(received_data))
})


# Testing that population columns are returned when requested\

test_that("Population columns are being received", {
  skip_on_cran()

  population_cols <- c("population_1km","population_2km","population_5km","population_best")

  received_data_pops <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                  country="Argentina", start_date="2022-01-01",end_date = "2022-01-04",
                                  population='full', prompt = F, acled_access = F, inter_numeric = TRUE)

  received_data_best <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                  country="Argentina", start_date="2022-01-01",end_date = "2022-01-04",
                                  population='best',prompt = F, acled_access = F, inter_numeric = TRUE)


  expect_true(all(population_cols %in% colnames(received_data_pops)))
  expect_true("population_best" %in% colnames(received_data_best))

})




# Errors ----
## Error when someone requests a region that does not exist----

test_that("Error prompted when region does not exist", {
  skip_on_cran()
  expect_error(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),regions = "Narnia",
                         start_date="2022-01-01",end_date = "2022-12-31",prompt = F, acled_access = F, log = F, inter_numeric = TRUE), regexp = "One or more requested region names not in the ACLED country list.")
})

test_that("Error when region number does not exist", {
  skip_on_cran()
  expect_error(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),regions = 420,
                         start_date="2022-01-01",end_date = "2022-12-31",prompt = F, acled_access = F, log = F, inter_numeric = TRUE),
               regexp = "One or more requested region numbers not in the ACLED country list")
})


## Errors when a country requested doesnt exists ----
test_that("Error when one of two countries are wrong",{
  skip_on_cran()
          expect_error(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),country = c("Argentia","Bolivia"),
                                 start_date="2022-01-01",end_date = "2022-12-31",prompt = F, acled_access = F, log = F, inter_numeric = TRUE),
                       regexp = "One or more of the requested *")})

## Test what happens when someone inputs acled_access as TRUE but it includes email and key. ----
test_that("Acled_access is ignored",{
  skip_on_cran()
  expect_true(grepl("acledrexamples", log_received_data_check_credential$email[1]))
})

# Test errors from incorrectly input arguments. ----

test_that("acled_api() throws an error when called with invalid arguments", {
  skip_on_cran()

  expect_error(acled_api(Country = "Argentina",
                         start_date="2022-01-01",
                         end_date = "2022-12-31",
                         prompt = F,
                         acled_access = T,
                         log = F, inter_numeric = TRUE), regexp=
                 "Country is not a valid option. Please utilize \"country\", without capitalizing")


  expect_error(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                         Region = "North America",
                         start_date="2022-01-01",
                         end_date = "2022-12-31",
                         prompt = F,
                         acled_access = T,
                         log = F, inter_numeric = TRUE), regexp=
               "Region is not a valid option. Please utilize \"regions\"")

  expect_error(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                         Regions = "North America",
                         start_date="2022-01-01",
                         end_date = "2022-12-31",
                         prompt = F,
                         acled_access = T,
                         log = F, inter_numeric = TRUE), regexp=
               "Regions is not a valid option. Please utilize \"regions\", without capitalizing")

  expect_error(acled_api(Event_type = "Argentina",
                         start_date="2022-01-01",
                         end_date = "2022-12-31",
                         prompt = F,
                         acled_access = T, inter_numeric = TRUE), regexp=
               "Event type is not a valid option. Please utilize \"event_types\", without capitalizing")
  expect_error(acled_api(country = "Argentina",
                         Start_date="2022-01-01",
                         end_date = "2022-12-31",
                         prompt = F,
                         acled_access = T, inter_numeric = TRUE), regexp=
               "Start_date is not a valid option. Please utilize \"start_date\", without capitalizing")
  expect_error(acled_api(country = "Argentina",
                         start_date="2022-01-01",
                         End_date = "2022-12-31",
                         prompt = F,
                         acled_access = T, inter_numeric = TRUE), regexp=
               "End_date is not a valid option. Please utilize \"end_date\", without capitalizing")
})

# Test errors from badly utilized acled_access and key/email combination----
test_that("If access is TRUE and credentials are null, credentials are ignored, but an error appears if Keys are empty in the enviornemt", {
  skip_on_cran()

  expect_error(
    Sys.setenv("acled_key" = "") %>%
      acled_api(Country = "Argentina",
                start_date="2022-01-01",
                end_date = "2022-12-31",
                prompt = F,
                acled_access = T,
                log = F, inter_numeric = TRUE), regexp = "acled_access is TRUE, but email and/or key are not stored in the enviornment. Please rerun acled_access or include key and email in function")
})

test_that("Users gets an error when acled_access is False, but no key or email are provided.", {
  skip_on_cran()

  expect_error(acled_api(country = "Argentina", start_date="2022-01-01",
                         end_date = "2022-12-31", prompt = F, acled_access = F, inter_numeric = TRUE), regexp = "Email address required")

  expect_error(acled_api(email = "stuff",country = "Argentina", start_date="2022-01-01",
                         end_date = "2022-12-31", prompt = F, acled_access = F, inter_numeric = TRUE), regexp = "Key required")



})


# Test error if start_date is after end_date ----
test_that("start_date is after end_date", {
  skip_on_cran()

  expect_error(
    acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
              country = "Argentina",
              start_date="2022-01-01",
              end_date = "2021-01-01",
              prompt = F,
              acled_access = F,
              log = F, inter_numeric = TRUE), regexp = "Requested \'start_date\'")})

# Error when timestamp is from a date later than today ----

test_that("timestamp is from a latter date than today." ,{
  skip_on_cran()

 expect_error(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                        country = "Argentina",
                        start_date="2021-01-01",
                        end_date = "2022-01-01",
                        prompt = F,
                        acled_access = F,
                        timestamp = paste0(year(now())+1, "01-01"), # Way to make it always in the future
                        log = F, inter_numeric = TRUE), regexp = "The timestamp cannot be" )
})

# Error when requesting non-existent event types ----

test_that("Error when non existent event types",{
  skip_on_cran()

  expect_error(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                         country = "Argentina",
                         start_date="2021-01-01",
                         end_date = "2022-01-01",
                         event_types = c("Protests","Superhero fight"),
                         prompt = F,
                         acled_access = F,
                         log = F, inter_numeric = TRUE), regexp = "One or more requested event types are not in the ACLED data.")
})

# A message appears that acled_access is being ignored, and the proper credentials are being used.----

test_that("A warning appears that acled_access is being ignored, and the proper credentials are being used.",{
  skip_on_cran()

  alog <- acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                              acled_access = T, log = T, inter_numeric = TRUE)

  expect_message(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                           acled_access = T, log = T, inter_numeric = TRUE), regexp = "acled_access is TRUE, but email and key are included in the function. Ignoring acled_access.")

  expect_true(grepl("acledrexamples", alog$email[1]))
})




# Weird cases ----

# Ensure tables do not display blanks, and display NAs instead

test_that("Tables display NAs instead of blanks", {
  skip_on_cran()

  expect_equal(nrow(filter(received_data, if_any(everything(), ~ sjmisc::is_empty(.x, all.na.empty = F)))), 0)


})

