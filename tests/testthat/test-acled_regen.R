# Test data type
test_that("function returns a tibble", {
  # test that the output is a tibble
  expect_s3_class(test_df, c("tbl_df","data.frame"))
})

# Test functionality
test_that("acled_regen function regenerates the event_id_no_cnty column correctly", {
  expected_output <- data.frame(
    event_id_cnty = c("AFG126", "SOM456", "ZWE789"),
    iso = c("AFG", "SOM", "ZWE"),
    event_id_no_cnty = c("126", "456", "789"),
    stringsAsFactors = FALSE
  )
  output <- acled_regen(test_df, "event_id_no_cnty")
  expect_identical(expected_output, output)
})

# Test errors
test_that("acled_regen function returns an error when dataframe argument is null", {
  expect_error(acled_regen(NULL, "event_id_no_cnty"), "Please indicate a dataframe where to add the column")
})

test_that("acled_regen function returns an error when column argument is null", {
  expect_error(acled_regen(test_df, NULL), "Please indicate a column in the column argument, make sure it is a string and without any typos")
})

test_that("acled_regen function returns an error when an invalid column argument is provided", {
  expect_error(acled_regen(test_df,"invalid_column"), "Column not found, please make sure that column is an available option.")
})
