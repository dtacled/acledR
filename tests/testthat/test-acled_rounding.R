test_that("acled_rounding behaves as expected", {
  # Test cases where acled_rounding and round give different results
  expect_equal(acled_rounding(1.5), 2)
  expect_equal(acled_rounding(2.5), 3)}
  )

test_that("acled_rounding gives the same result as round when rounding .6+",{
  # Test case where acled_rounding and round give the same result
  expect_equal(acled_rounding(2.6), 3)}
  )

test_that("Rounding to a specific decimal place",{

  expect_equal(acled_rounding(1.56, 1), 1.6)
  expect_equal(acled_rounding(2.34, 1), 2.3)}
  )

test_that("Rounding to a specific decimal place where acled_rounding and round give different results",{
  expect_equal(acled_rounding(2.45, 1), 2.5)}
  )

test_that("Works on negative numbers", {
  expect_equal(acled_rounding(-2.5), -2)}
  )

test_that("Integer is fine", {
  # Test input that's already an integer
  expect_equal(acled_rounding(2), 2)})

