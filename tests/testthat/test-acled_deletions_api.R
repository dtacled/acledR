# Test for acled_deletions_api

# Basic check
test_that("number of columns is correct - date", {
  expect_equal(ncol(received_deleted_data_date),2)
})

test_that("number of columns is correct - unix", {
  expect_equal(ncol(received_deleted_data_unix),2)
})

test_that("names of columns are correct - date", {
  expect_equal(names(received_deleted_data_date),columns)
})

test_that("names of columns are correct - unix", {
  expect_equal(names(received_deleted_data_unix),columns)
})

# Date and unix are the same
test_that("names of columns are correct - unix", {
  expect_true(all.equal(received_deleted_data_date, received_deleted_data_unix))
})
