skip_on_cran()

# Check that it produces the same result as intended


test_that("The function swaps the interaction codes properly",{
  skip_on_cran()

  expect_equal(acled_transform_interaction(test, only_inters = F), test_changes)

})


# Errors and warnings  ----


# Error when there is a wrong data structure
test_that("Returns an error when the function receives a df without inter1 or inter2", {
    skip_on_cran()

    expect_error(acled_transform_interaction(dplyr::select(test,-inter1)), regexp = "The input dataframe does not contain 'inter1' column")

    expect_error(acled_transform_interaction(dplyr::select(test,-inter2)), regexp = "The input dataframe does not contain 'inter2' column")

    expect_error(acled_transform_interaction(dplyr::select(test,-interaction)), regexp = "The input dataframe does not contain 'interaction' column")
})

# Error where are inter codes that are not recognized

test_that("Returns an error when there are unrecognized inter codes", {
  skip_on_cran()


  test3 <- test
  test3[1,10] <- 9

  expect_error(acled_transform_interaction(test3), regexp = "One or more interaction codes were not recognized.")
})



