# Test for acled_deletions_api

# Basic check ----

test_that("number of columns is correct - date", {
  skip_on_cran()
  expect_equal(ncol(received_deleted_data_date), 2)
})


test_that("number of columns is correct - unix", {
  skip_on_cran()
  expect_equal(ncol(received_deleted_data_unix),2)
})

test_that("names of columns are correct - date", {
  skip_on_cran()
  expect_equal(names(received_deleted_data_date), columns_deleted)
})

test_that("names of columns are correct - unix", {
  skip_on_cran()
  expect_equal(names(received_deleted_data_unix), columns_deleted)
})


test_that("names of columns are correct - unix", {
  skip_on_cran()

  expect_true(all.equal(received_deleted_data_date, received_deleted_data_unix))
})






