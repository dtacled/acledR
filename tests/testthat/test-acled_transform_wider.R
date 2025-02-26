

# Tests for proper functioning of the function (Can they return the equivalent of what is acled_transform_longer input)----

test_that("acled_transform_wider returns expected results for type = 'full_actors'", {
  skip_on_cran()

  # Define a simple acledR::acled_old_dummy frame

  # Transform acledR::acled_old_dummy from wide to long format
  transformed_data <- acled_transform_longer(acledR::acled_old_dummy, type = "full_actors")

  # Transform acledR::acled_old_dummy from long to wide format
  reversed_data <- acled_transform_wider(transformed_data, type = "full_actors") %>%
    mutate(actor2 = na_if(actor2, ""))

  # Test if the original acledR::acled_old_dummy and the reversed acledR::acled_old_dummy are the same
  expect_equal(dplyr::arrange(acledR::acled_old_dummy,event_id_cnty), dplyr::arrange(reversed_data, event_id_cnty),
               ignore_attr = TRUE)
})

test_that("acled_transform_wider returns expected results for type = 'main_actors'", {
  skip_on_cran()

  # Define a simple acledR::acled_old_dummy frame

  # Transform acledR::acled_old_dummy from wide to long format
  transformed_data <- acled_transform_longer(acledR::acled_old_dummy, type = "main_actors")

  # Transform acledR::acled_old_dummy from long to wide format
  reversed_data <- acled_transform_wider(transformed_data, type = "main_actors")

  # Test if the original acledR::acled_old_dummy and the reversed acledR::acled_old_dummy are the same
  expect_equal(dplyr::arrange(acledR::acled_old_dummy,event_id_cnty), dplyr::arrange(reversed_data, event_id_cnty),
               ignore_attr = TRUE)
})

test_that("acled_transform_wider returns expected results for type = 'assoc_actors'", {
  skip_on_cran()

  # Transform acledR::acled_old_dummy from wide to long format
  transformed_data <- acled_transform_longer(acledR::acled_old_dummy, type = "assoc_actors")

  # Transform acledR::acled_old_dummy from long to wide format
  reversed_data <- acled_transform_wider(transformed_data, type = "assoc_actors")

  # Test if the original acledR::acled_old_dummy and the reversed acledR::acled_old_dummy are the same
  expect_equal(dplyr::arrange(acledR::acled_old_dummy,event_id_cnty), dplyr::arrange(reversed_data, event_id_cnty),
               ignore_attr = TRUE)
})

test_that("acled_transform_wider returns expected results for type = 'source'", {
  skip_on_cran()

  # Transform acledR::acled_old_dummy from wide to long format
  transformed_data <- acled_transform_longer(acledR::acled_old_dummy, type = "source")

  # Transform acledR::acled_old_dummy from long to wide format
  reversed_data <- acled_transform_wider(transformed_data, type = "source")

  # Test if the original acledR::acled_old_dummy and the reversed acledR::acled_old_dummy are the same
  expect_equal(dplyr::arrange(acledR::acled_old_dummy,event_id_cnty), dplyr::arrange(reversed_data, event_id_cnty),
               ignore_attr = TRUE)
})

# Tests for proper errors and messages----

## Test if function returns an error when a data frame with missing necessary columns is input----
test_that("Function returns error with missing columns", {
  skip_on_cran()

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

## Test if function returns an error when a non-existent type is input----
test_that("Function returns NULL when non-existent type is input", {
  skip_on_cran()

  df <- data.frame(actor = c("a", "b"), type_of_actor = c(1, 2), inter_type = c(1, 2), inter = c(1, 2))

  expect_error(acled_transform_wider(df, type = "non_existent_type"), regexp = "is not a valid option.")
})



# Weird cases. ----

## Test that if you request monadic data from the API, you can convert it back. ----

test_that("Can you request data from the monadic api, and convert it back?", {
  skip_on_cran()

  tester <- acledR::acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                              country = "Brazil", start_date="2022-01-01",
                              end_date = "2022-12-31", monadic = T,
                              prompt = F, acled_access = F, log = F, inter_numeric = TRUE)%>%
    acled_transform_wider(type = "api_monadic")

  control <- acledR::acled_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                              country = "Brazil", start_date="2022-01-01",
                              end_date = "2022-12-31", monadic = F,
                              prompt = F, acled_access = F, log = F, inter_numeric = TRUE)%>%
    arrange(desc(event_id_cnty))


  expect_equal(tester, control)

})

## Test that if you use the function with some additional transformations made to the dataset, you can transform back without losing any data. ----
