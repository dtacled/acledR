

# Test data type ----
# Test that the function returns a tibble
test_that("returns a tibble", {
  skip_on_cran()

  data <- data.frame(actor1 = c("Actor D"), actor2 = c("Actor C"), assoc_actor_1 = c("Actor A; Actor B"), assoc_actor_2 = c("Hello"),inter1 = "1", inter2="2",sub_event_type = "Protests", source_scale= "khapow",source= 'khapow')
  output <- acled_transform_longer(data, "full_actors")
  expect_s3_class(output, c("tbl_df", "data.frame"))
})

# Test functionalities ----
# Test that the function returns the correct number of rows for full_actors type
test_that("returns correct number of rows for full_actors type", {
  skip_on_cran()

  data <- data.frame(actor1 = c("Actor D"), actor2 = c("Actor C"), assoc_actor_1 = c("Actor A; Actor B"), assoc_actor_2 = c("Hello"), inter1 = "1", inter2="2",sub_event_type = "Protests", source_scale= "khapow",source= 'khapow')
  output <- acled_transform_longer(data, "full_actors")
  expect_equal(nrow(output), 5)
})

# Test that the function returns the correct number of rows for main_actors type
test_that("returns correct number of rows for main_actors type", {
  skip_on_cran()

  data <- data.frame(actor1 = c("Actor D"), actor2 = c("Actor C"), assoc_actor_1 = c("Actor A; Actor B"), assoc_actor_2 = c(""),inter1 = "1", inter2="2", sub_event_type = "Protests", source_scale= "khapow",source= 'khapow')
  output <- acled_transform_longer(data, "main_actors")
  expect_equal(nrow(output), 2)
})

# Test that the function returns the correct number of rows for assoc_actors type
test_that("returns correct number of rows for assoc_actors type", {
  skip_on_cran()

  data <- data.frame(actor1 = c("Actor D"), actor2 = c("Actor C"), assoc_actor_1 = c("Actor A; Actor B"), assoc_actor_2 = c("hello"), inter1 = "1", inter2="2",sub_event_type = "Protests", source_scale= "khapow",source= 'khapow')
  output <- acled_transform_longer(data, "assoc_actors")
  expect_equal(nrow(output), 3)
})

# Test that the function returns the correct number of rows for source type
test_that("returns correct number of rows for source type", {
  skip_on_cran()

  data <- data.frame(actor1 = c("Actor D"), actor2 = c("Actor C"), assoc_actor_1 = c("Actor A; Actor B"), assoc_actor_2 = c("Hello"), inter1 = "1", inter2="2",sub_event_type = "Protests", source_scale= "khapow", source = c("Source A; Source B"))
  output <- acled_transform_longer(data, "source")
  expect_equal(nrow(output), 2)
})


# Test errors----
# Test that the function throws an error for an invalid type parameter
test_that("throws an error for invalid type parameter", {
  skip_on_cran()

  data <- data.frame(actor1 = c("Actor A; Actor B"), actor2 = c("Actor C"), assoc_actor_1 = c("Actor D"), assoc_actor_2 = c(""),inter1 = "1", inter2="2")
  expect_error(acled_transform_longer(data, "invalid_type"))
})

# Test that if columns are missing, it will throw an error for an invalid data frame

test_that("if columns are missing, it will throw an error for an invalid data frame", {
  skip_on_cran()

  data <- data.frame(actor1 = c("Actor A; Actor B"), actor2 = c("Actor C"),inter1 = "1", inter2="2", assoc_actor_1 = c("Actor D"), assoc_actor_2 = c(""))
  expect_error(acled_transform_longer(data, "full_actors"), "Some columns are missing. Please make sure your data frame includes: actor1, actor2, assoc_actor_1, assoc_actor_2, sub_event_type, source_scale, source.")
})

# Test that you get an error if the actor1 or actor 2 columns have more than one value per row.
test_that("error if the actor1 or actor 2 columns have more than one value per row", {
  skip_on_cran()

  data <- data.frame(actor1 = c("Actor A; Actor B"), actor2 = c("Actor C"), inter1 = "1", inter2="2",assoc_actor_1 = c("Actor D"), assoc_actor_2 = c(""), sub_event_type = "Protests", source_scale= "", source = c("Source A; Source B"))
  expect_error(acled_transform_longer(data, "main_actors", "*column seems to include more than one result per row. That is inconsistent with our column structure."))
})

test_that("warning when there are empty rows in the assoc actors column", {
  skip_on_cran()

  data <- data.frame(actor1 = c("Actor D"), actor2 = c("Actor C"), inter1 = "1", inter2="2",assoc_actor_1 = c("Actor A; Actor B"), assoc_actor_2 = c(""), sub_event_type = "Protests", source_scale= "",source= '')
  expect_warning(acled_transform_longer(data, "assoc_actors"), "There are empty rows in the assoc_actor column.")
  expect_warning(acled_transform_longer(data, "full_actors"), "There are empty rows in the actor column.")
})
