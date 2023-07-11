test_that("acled_reverse_transform returns expected results for type = 'full_actors'", {

  # Define a simple data frame

  # Transform data from wide to long format
  transformed_data <- acled_transform_wide_to_long(data, type = "full_actors")

  # Transform data from long to wide format
  reversed_data <- acled_reverse_transform(transformed_data, type = "full_actors")

  # Test if the original data and the reversed data are the same
  expect_equal(dplyr::arrange(data), dplyr::arrange(reversed_data))
})


test_that("acled_reverse_transform returns expected results for type = 'main_actors'", {

  # Define a simple data frame
  data <- tibble::tibble(
    actor1 = c("Actor A", "Actor B"),
    actor2 = c("Actor C", "Actor D"),
    sub_event_type = c("Event 1", "Event 2"),
    source_scale = c("Local", "Global"),
    source = c("Source 1", "Source 2")
  )

  # Transform data from wide to long format
  transformed_data <- acled_transform_wide_to_long(data, type = "main_actors")

  # Transform data from long to wide format
  reversed_data <- acled_reverse_transform(transformed_data, type = "main_actors")

  # Test if the original data and the reversed data are the same
  expect_equal(dplyr::arrange(data), dplyr::arrange(reversed_data))
})

test_that("acled_reverse_transform returns expected results for type = 'assoc_actors'", {

  # Define a simple data frame
  data <- tibble::tibble(
    assoc_actor_1 = c("Actor E;Actor F", "Actor G;Actor H"),
    assoc_actor_2 = c("Actor I", "Actor J"),
    sub_event_type = c("Event 1", "Event 2"),
    source_scale = c("Local", "Global"),
    source = c("Source 1", "Source 2")
  )

  # Transform data from wide to long format
  transformed_data <- acled_transform_wide_to_long(data, type = "assoc_actors")

  # Transform data from long to wide format
  reversed_data <- acled_reverse_transform(transformed_data, type = "assoc_actors")

  # Test if the original data and the reversed data are the same
  expect_equal(dplyr::arrange(data), dplyr::arrange(reversed_data))
})

test_that("acled_reverse_transform returns expected results for type = 'source'", {

  # Define a simple data frame
  data <- tibble::tibble(
    source_scale = c("Local", "Global"),
    source = c("Source 1;Source 2", "Source 3;Source 4"),
    sub_event_type = c("Event 1", "Event 2")
  )

  # Transform data from wide to long format
  transformed_data <- acled_transform_wide_to_long(data, type = "source")

  # Transform data from long to wide format
  reversed_data <- acled_reverse_transform(transformed_data, type = "source")

  # Test if the original data and the reversed data are the same
  expect_equal(dplyr::arrange(data), dplyr::arrange(reversed_data))
})

test_that("acled_reverse_transform returns expected results for type = 'all'", {

  # Define a simple data frame
  data <- tibble::tibble(
    actor1 = c("Actor A", "Actor B"),
    actor2 = c("Actor C", "Actor D"),
    assoc_actor_1 = c("Actor E;Actor F", "Actor G;Actor H"),
    assoc_actor_2 = c("Actor I", "Actor J"),
    sub_event_type = c("Event 1", "Event 2"),
    source_scale = c("Local", "Global"),
    source = c("Source 1;Source 2", "Source 3;Source 4")
  )

  # Transform data from wide to long format
  transformed_data <- acled_transform_wide_to_long(data, type = "all")

  # Transform data from long to wide format
  reversed_data <- acled_reverse_transform(transformed_data, type = "all")

  # Test if the original data and the reversed data are the same
  expect_equal(dplyr::arrange(data), dplyr::arrange(reversed_data))
})
