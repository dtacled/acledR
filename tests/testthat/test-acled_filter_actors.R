

# Test operation----
## Test that the function returns a tibble when given a valid input. ----
test_that("function returns a tibble", {
  # create sample input data
  df <- tibble(actor1 = c("A", "B"), actor2 = c("C", "D"),
               assoc_actor_1 = c("X", "Y"), assoc_actor_2 = c("Z", "W"))
  actors <- c("A", "B")

  # call the function
  filtered_df <- acled_filter_actors(df, actors)

  # test that the output is a tibble
  expect_s3_class(filtered_df, "tbl_df")
})


## Test if the function returns the expected output when filtering by all actor columns ----
test_that("Filter by all actors returns the expected output", {
  # Create a mock dataframe
  df <- tibble::tribble(
    ~actor1,                       ~actor2,                                   ~assoc_actor_1,                                   ~assoc_actor_2,
    'Military Forces of Yemen',    'Yemeni Houthi rebels (Ansar Allah)',      'Al-Qaida in the Arabian Peninsula (AQAP)',      '',
    'Yemeni Houthi rebels (Ansar Allah)', 'Military Forces of Yemen',          '',                                             '',
    'Al-Qaida in the Arabian Peninsula (AQAP)', '',                           '',                                             ''
  )
  actors <- c('Military Forces of Yemen', 'Yemeni Houthi rebels (Ansar Allah)')
  expected_df <- df[1:2, ]
  actual_df <- acled_filter_actors(df = df, actors = actors, filter_cols = 'all')
  # Compare the expected output to the actual output
  expect_equal(actual_df, expected_df)
})

## Test that the function filters the ACLED data by actor1.----
test_that("filters by actor1", {
  df <- data.frame(actor1 = c('Actor 1', 'Actor 2'), actor2 = c('Actor 3', 'Actor 4'),
                   assoc_actor_1 = c('Actor 5; Actor 6', 'Actor 7'), assoc_actor_2 = c('Actor 8', 'Actor 9'))
  actors <- 'Actor 1'
  filtered_df <- acled_filter_actors(df, actors, filter_cols='primary')
  expect_equal(nrow(filtered_df), 1)
  expect_equal(filtered_df$actor1[1],'Actor 1')
})

## Test that the function filters the ACLED data by actor2. ----
test_that("filters by actor2", {
  df <- data.frame(actor1 = c('Actor 1', 'Actor 2'), actor2 = c('Actor 3', 'Actor 4'),
                   assoc_actor_1 = c('Actor 5; Actor 6', 'Actor 7'), assoc_actor_2 = c('Actor 8', 'Actor 9'))
  actors <- 'Actor 3'
  filtered_df <- acled_filter_actors(df, actors, filter_cols='primary')
  expect_equal(nrow(filtered_df), 1)
  expect_equal(filtered_df$actor2[1], 'Actor 3')
})
## Test that the function filters the ACLED data by assoc_actor_1 and assoc_actor_2. ----
test_that("filters by assoc_actor_1 and assoc_actor_2", {
  df <- data.frame(actor1 = c('Actor 1', 'Actor 2'), actor2 = c('Actor 3', 'Actor 4'),
                   assoc_actor_1 = c('Actor 5; Actor 6', 'Actor 7'), assoc_actor_2 = c('Actor 8', 'Actor 9'))
  actors <- 'Actor 5'
  filtered_df <- acled_filter_actors(df, actors, filter_cols='all')
  expect_equal(nrow(filtered_df), 1)
  expect_true(stringr::str_detect(filtered_df$assoc_actor_1, actors))
})



# Error related ----
## Test if function throws error if dataframe is missing 'assoc_actor_1' or 'assoc_actor_2' column ----
test_that("function throws error if dataframe is missing 'assoc_actor_1' or 'assoc_actor_2' column", {
  df <- data.frame(actor1 = c("A", "B"), actor2 = c("C", "D"))
  expect_error(acled_filter_actors(df, actors = c("A"), filter_cols = 'all'), "Dataframe is missing 'assoc_actor_1' or 'assoc_actor_2' column.")
})

## Test if function throws error if filter_cols argument is not 'all' or 'primary' ----
test_that("function throws error if filter_cols argument is not 'all' or 'primary'", {
  df <- data.frame(actor1 = c("A", "B"), actor2 = c("C", "D"))
  expect_error(acled_filter_actors(df, actors = c("A"), filter_cols = 'All'),"filter_cols argument must be 'all' or 'primary'.")
})



# Regex related ----
## Test with multiple actor names and parentheses
test_that("Test that regex can handle multiple actor names and parentheses",{
  df <- data.frame(
    actor1 = c("Actor A", "Actor B"),
    actor2 = c("Actor C", "Actor D"),
    assoc_actor_1 = c("Actor A (Group 1); Actor E", "Actor F; Actor B (Group 2)"),
    assoc_actor_2 = c("Actor G", "Actor H; Actor C (Group 3)"))
  actors <- c("Actor A (Group 1)", "Actor C (Group 3)")
  filtered_df <- acled_filter_actors(df, actors)
  expect_equal(nrow(filtered_df), 2)
})
