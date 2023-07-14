# Tests for proper functioning of the function (Can they return the equivalent of what is acled_transform_longer input)

test_that("acled_transform_wider returns expected results for type = 'full_actors'", {

  # Define a simple acledR::acled_old_dummy frame

  # Transform acledR::acled_old_dummy from wide to long format
  transformed_data <- acled_transform_longer(acledR::acled_old_dummy, type = "full_actors")

  # Transform acledR::acled_old_dummy from long to wide format
  reversed_data <- acled_transform_wider(transformed_data, type = "full_actors") %>%
    mutate(actor2 = na_if(actor2, ""))

  # Test if the original acledR::acled_old_dummy and the reversed acledR::acled_old_dummy are the same
  expect_equal(dplyr::arrange(acledR::acled_old_dummy,event_id_cnty), dplyr::arrange(reversed_data, event_id_cnty))
})

test_that("acled_transform_wider returns expected results for type = 'main_actors'", {

  # Define a simple acledR::acled_old_dummy frame

  # Transform acledR::acled_old_dummy from wide to long format
  transformed_data <- acled_transform_longer(acledR::acled_old_dummy, type = "main_actors")

  # Transform acledR::acled_old_dummy from long to wide format
  reversed_data <- acled_transform_wider(transformed_data, type = "main_actors")

  # Test if the original acledR::acled_old_dummy and the reversed acledR::acled_old_dummy are the same
  expect_equal(dplyr::arrange(acledR::acled_old_dummy,event_id_cnty), dplyr::arrange(reversed_data, event_id_cnty))
})

test_that("acled_transform_wider returns expected results for type = 'assoc_actors'", {

  # Transform acledR::acled_old_dummy from wide to long format
  transformed_data <- acled_transform_longer(acledR::acled_old_dummy, type = "assoc_actors")

  # Transform acledR::acled_old_dummy from long to wide format
  reversed_data <- acled_transform_wider(transformed_data, type = "assoc_actors")

  # Test if the original acledR::acled_old_dummy and the reversed acledR::acled_old_dummy are the same
  expect_equal(dplyr::arrange(acledR::acled_old_dummy,event_id_cnty), dplyr::arrange(reversed_data, event_id_cnty))
})

test_that("acled_transform_wider returns expected results for type = 'source'", {

  # Transform acledR::acled_old_dummy from wide to long format
  transformed_data <- acled_transform_longer(acledR::acled_old_dummy, type = "source")

  # Transform acledR::acled_old_dummy from long to wide format
  reversed_data <- acled_transform_wider(transformed_data, type = "source")

  # Test if the original acledR::acled_old_dummy and the reversed acledR::acled_old_dummy are the same
  expect_equal(dplyr::arrange(acledR::acled_old_dummy,event_id_cnty), dplyr::arrange(reversed_data, event_id_cnty))
})

# Tests for proper errors and messages

# Test if function returns an error when a data frame with missing necessary columns is input
test_that("Function returns error with missing columns", {

  df <- data.frame(a = c(1, 2, 3), b = c("a", "b", "c"))

  expect_error(acled_transform_wider(df, "full_actors"),
               "Some columns are missing. Please make sure your data frame includes: actor,type_of_actor,inter_type, and inter.")

  expect_error(acled_transform_wider(df, "main_actors"),
               "Some columns are missing. Please make sure your data frame includes: actor,type_of_actor,inter_type, and inter.")

  expect_error(acled_transform_wider(df, "assoc_actors"),
               "Some columns are missing. Please make sure your data frame includes: assoc_actor,type_of_assoc_actor.")

  expect_error(acled_transform_wider(df, "source"),
               "Some columns are missing. Please make sure your data frame includes: source")
})

# Test if function returns an error when a non-existent type is input
test_that("Function returns NULL when non-existent type is input", {

  df <- data.frame(actor = c("a", "b"), type_of_actor = c(1, 2), inter_type = c(1, 2), inter = c(1, 2))

  expect_equal(acled_transform_wider(df, "non_existent_type"), NULL)
})

