# acled_api test


test_that("number of columns is correct", {
  expect_equal(ncol(received_data),31)
})

test_that("names of columns are correct", {
  expect_equal(names(received_data),columns)
})

test_that("Split calls for big calls", {
  expect_equal(as.numeric(ceiling(sum(log_received_data$time)/300000)),max(log_received_data$calls))
})

test_that("Error when one of two countries are wrong",{
          expect_error(acled_api(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5",countries = c("Argentia","Bolivia"),
                                 start_date="2022-01-01",end_date = "2022-12-31",prompt = F, acled_access = F, log = F),
                       regexp = "One or more of the requested countries *")})


# Tests to test

test_that("Regions in numeric work",{
  expect_true(all.equal(data.frame(region="Western Africa",rows=1:nrow(received_data_numeric_region))$region,received_data_numeric_region$region))})


# Test what happends when someone requests a region and a country of another region
