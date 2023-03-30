
# Test data type ----

# Test 1: Check if the function returns a tibble or data frame
test_that("Function returns a tibble", {
  expect_s3_class(acled_filter_event_types(mock_data, c("demonstrations")), c("tbl_df","data.frame"))
})


# Test functionality ----
# Test 2: Check if the function returns the correct columns
test_that("Function returns correct columns", {
  expect_identical(colnames(acled_filter_event_types(mock_data, c("demonstrations"))), c("event_type", "sub_event_type","fatalities", "demonstrations"))
})

# Test 3: Check if the function filters correctly
test_that("Function filters correctly", {
  expect_identical(nrow(acled_filter_event_types(mock_data, c("demonstrations"))), 2L)
})

# Test 4: Check if the function retains all events when keep_all_events = TRUE
test_that("Function retains all events when keep_all_events = TRUE", {
  expect_identical(nrow(acled_filter_event_types(mock_data, c("demonstrations"), keep_all_events = TRUE)), nrow(mock_data))
})

# Test errors ----

# Test 5: Check if the function throws an error if the 'event_type' column is not present
test_that("Function throws error if 'event_type' column is not present", {
  expect_error(acled_filter_event_types(data.frame(other_column = 1), c("demonstrations")), "Column 'event_type' not in data.")
})

# Test 6: Check if the function throws an error if the 'sub_event_type' column is not present
test_that("Function throws error if 'sub_event_type' column is not present", {
  expect_error(acled_filter_event_types(data.frame(event_type = "disorder"), c("demonstrations")), "Please use data with columns for both ACLED 'event_type' and 'sub_event_type'.")
})

# Test 7: Check if the function throws an error if an invalid category is requested
test_that("Function throws error if invalid category is requested", {
  expect_error(acled_filter_event_types(mock_data, c("invalid_category")), "Requested 'acled_category' is not a designated option.")
})

