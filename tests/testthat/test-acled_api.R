# acled_api unit testing


test_that("number of columns is correct", {
  skip_on_cran()
  expect_equal(ncol(received_data),31)
})

test_that("names of columns are correct", {
  skip_on_cran()
  expect_equal(names(received_data),columns)
})


test_that("event_type filters work or not (single)",{
  skip_on_cran()
  expect_equal(unique(acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                password = Sys.getenv("ACLED_API_PASSWORD"),
                                start_date="2022-01-01",end_date = "2022-02-01", country = "Argentina",
                                event_type = "Protests",
                                inter_numeric = TRUE)$event_type), "Protests" )

})

test_that("event_type filters work or not (multiple)",{
  skip_on_cran()
  expect_equal(unique(acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                password = Sys.getenv("ACLED_API_PASSWORD"),
                                start_date="2022-01-01",end_date = "2022-02-01", country = "Argentina",
                                event_type = c("Protests", "Riots"),
                                inter_numeric = TRUE)$event_type), c("Riots", "Protests"))

})

test_that("country filters work or not (multiple)",{
  skip_on_cran()
  expect_equal(unique(acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                password = Sys.getenv("ACLED_API_PASSWORD"),
                                start_date="2022-01-01",
                                end_date = "2022-02-01",
                                country = c("Argentina", "Peru"),
                                inter_numeric = TRUE)$country), c("Argentina", "Peru"))

})

test_that("country filters work with event_type filters (multiple)",{
  skip_on_cran()
  df_country_et_filters <- acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                     password = Sys.getenv("ACLED_API_PASSWORD"),
                                     start_date="2022-01-01",
                                     end_date = "2022-02-01",
                                     country = c("Argentina", "Peru"),
                                     event_type = c("Protests", "Riots"),
                                     inter_numeric = TRUE)

  expect_equal(unique(df_country_et_filters$country), c("Argentina", "Peru"))
  expect_equal(unique(df_country_et_filters$event_type), c("Riots", "Protests"))

})



# Regions are managed properly ----
test_that("Regions in numeric work",
          {
            skip_on_cran()
            expect_true(all.equal(data.frame(region="South Asia", rows=1:nrow(received_data_numeric_region))$region,
                                  received_data_numeric_region$region))
          }
)


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


test_that("timestamp (string) actually gets used as filter", {
  skip_on_cran()
  expect_gte(min(timestamp_string_check$timestamp), 1673295342)
})



# When asking for monadics, it returns monadics ----

test_that("The call actually returns monadics.", {
  skip_on_cran()
  expect_equal(min(received_data_monadic$event_date), min(received_data$event_date))

  expect_equal(max(received_data_monadic$event_date), max(received_data$event_date))

  expect_equal(unique(received_data_monadic$country), unique(received_data$country))

  expect_gte(nrow(received_data_monadic), nrow(received_data))
})


# Testing that population columns are returned when requested

test_that("Population columns are being received", {
  skip_on_cran()

  population_cols <- c("population_1km","population_2km","population_5km","population_best")


  received_data_pops <- acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                      password = Sys.getenv("ACLED_API_PASSWORD"),
                                  country="Argentina", start_date="2022-01-01",end_date = "2022-01-04",
                                  population='full',
                                  inter_numeric = TRUE)

  received_data_best <- acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                                      password = Sys.getenv("ACLED_API_PASSWORD"),
                                  country="Argentina", start_date="2022-01-01",end_date = "2022-01-04",
                                  population='best',
                                  inter_numeric = TRUE)


  expect_true(all(population_cols %in% colnames(received_data_pops)))
  expect_true("population_best" %in% colnames(received_data_best))

})




# Errors ----
## Error when someone requests a region that does not exist----

test_that("Error prompted when region does not exist", {
  skip_on_cran()
  expect_error(acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                         password = Sys.getenv("ACLED_API_PASSWORD"),
                         regions = "Narnia",
                         start_date="2022-01-01",end_date = "2022-12-31",
                         inter_numeric = TRUE), regexp = "One or more requested region names not in the ACLED country list.")
})

test_that("Error when region number does not exist", {
  skip_on_cran()

  expect_error(acled_api(email = Sys.getenv("ACLED_API_EMAIL"),
                         password = Sys.getenv("ACLED_API_PASSWORD"),
                         regions = 420,
                         start_date="2022-01-01",end_date = "2022-12-31",
                         inter_numeric = TRUE),
               regexp = "One or more requested region numbers not in the ACLED country list")
})


## Errors when a country requested doesnt exists ----
test_that("Error when one of two countries are wrong",{
  skip_on_cran()

  expect_error(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"),
                         password = Sys.getenv("ACLED_API_PASSWORD"),
                         country = c("Argentia", "Bolivia"),
                         start_date="2022-01-01",end_date = "2022-12-31",
                         inter_numeric = TRUE),
               regexp = "One or more of the requested *")
  }
)

## Test what happens when someone inputs acled_access as TRUE but it includes email and key. ----

# Test errors from incorrectly input arguments. ----

test_that("acled_api() throws an error when called with invalid arguments", {
  skip_on_cran()

  expect_error(acled_api(Country = "Argentina",
                         start_date="2022-01-01",
                         end_date = "2022-12-31",
                         email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"),
                         password = Sys.getenv("ACLED_API_PASSWORD"),
                         inter_numeric = TRUE), regexp=
                 "Country is not a valid option. Please utilize \"country\", without capitalizing")


  expect_error(acled_api(Region = "North America",
                         start_date="2022-01-01",
                         end_date = "2022-12-31",
                         email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"),
                         password = Sys.getenv("ACLED_API_PASSWORD"),
                         inter_numeric = TRUE), regexp=
               "Region is not a valid option. Please utilize \"regions\"")

  expect_error(acled_api(Regions = "North America",
                         start_date="2022-01-01",
                         end_date = "2022-12-31",
                         email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"),
                         password = Sys.getenv("ACLED_API_PASSWORD"),
                         inter_numeric = TRUE), regexp=
               "Regions is not a valid option. Please utilize \"regions\", without capitalizing")

  expect_error(acled_api(Event_type = "Argentina",
                         start_date="2022-01-01",
                         end_date = "2022-12-31",
                         email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"),
                         password = Sys.getenv("ACLED_API_PASSWORD"),
                         inter_numeric = TRUE), regexp=
               "Event type is not a valid option. Please utilize \"event_types\", without capitalizing")
  expect_error(acled_api(country = "Argentina",
                         Start_date="2022-01-01",
                         end_date = "2022-12-31",
                         email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"),
                         password = Sys.getenv("ACLED_API_PASSWORD"),
                         inter_numeric = TRUE), regexp=
               "Start_date is not a valid option. Please utilize \"start_date\", without capitalizing")
  expect_error(acled_api(country = "Argentina",
                         start_date="2022-01-01",
                         End_date = "2022-12-31",
                         email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"),
                         password = Sys.getenv("ACLED_API_PASSWORD"),
                         inter_numeric = TRUE), regexp=
               "End_date is not a valid option. Please utilize \"end_date\", without capitalizing")
})


# Test error if start_date is after end_date ----
test_that("start_date is after end_date", {
  skip_on_cran()

  expect_error(
    acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"),
              password = Sys.getenv("ACLED_API_PASSWORD"),
              country = "Argentina",
              start_date="2022-01-01",
              end_date = "2021-01-01",
              inter_numeric = TRUE), regexp = "Requested \'start_date\'")})

# Error when timestamp is from a date later than today ----

test_that("timestamp is from a latter date than today." ,{
  skip_on_cran()

 expect_error(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"),
                        password = Sys.getenv("ACLED_API_PASSWORD"),
                        country = "Argentina",
                        start_date="2021-01-01",
                        end_date = "2022-01-01",
                        timestamp = paste0(year(now()) + 1, "01-01"), # Way to make it always in the future
                        inter_numeric = TRUE), regexp = "The timestamp cannot be" )
})

# Error when requesting non-existent event types ----

test_that("Error when non existent event types",{
  skip_on_cran()

  expect_error(acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"),
                         password = Sys.getenv("ACLED_API_PASSWORD"),
                         country = "Argentina",
                         start_date="2021-01-01",
                         end_date = "2022-01-01",
                         event_types = c("Protests","Superhero fight"),
                         inter_numeric = TRUE), regexp = "One or more requested event types are not in the ACLED data.")
})
