
# Returns a data.frame structure ----
test_that("Returns a tibble", {
  counts <- acled_generate_counts(data, unit_id = "country", time_id = "event_date", time_target = "week") # nolint: line_length_linter.
  expect_s3_class(counts, "data.frame")
})

# Proper functioning ----
## Returns correct columns
test_that("Returns correct total event counts for months", {
  counts_2 <- acled_generate_counts(data,
                                    unit_id = "country",
                                    time_id = "event_date",
                                    time_target = "month")
  # Check with the old school version
  expect_equal(sum(data_count_month$n), sum(counts_2$total_events))
})

test_that("Returns correct total of weekly events", {
  weekly_tbilisi <- acled_generate_counts(data,
                                                event_type = c("Protests"),
                                                unit_id = "admin1",
                                                time_id = "event_date",
                                                time_target = "week") %>%
                                              filter(admin1 == "Tbilisi")
  expect_equal(sum(weekly_tbilisi$protests),
               sum(non_count_week_tbilisi$protests))
})

# Can handle multiple filters, and sum them as intended
test_that("Handles multiple event types correctly", {
  counts <- acled_generate_counts(data, event_type = c("Riots", "Protests"),
                                 unit_id = "admin1",
                                 time_id = "event_date",
                                 time_target = "week") %>%
                                 filter(admin1 == "Tbilisi")
  expect_true(all(c("riots", "protests") %in% colnames(counts)))
  #  Actual number of columns is here
  expect_equal(ncol(counts), 6)

  # Proper count of events (1)
  expect_equal(sum(counts$protests),
               sum(weekly_protests_tbilisi$protests))
  # Proper count of events(2)
  expect_equal(sum(counts$riots),
               sum(weekly_riots_tbilisi$riots))
})

# It adds the units ids of empty units.
test_that("Returns correct counts when add_unit_ids is used", {
  counts <- acled_generate_counts(data,
                                  unit_id = "country", event_type = "Riots",
                                  time_id = "event_date", time_target = "week",
                                  start_date = min(data$event_date),
                                  end_date = "2022-02-01",
                                  add_unit_ids = "Finland")
  expect_equal(sum(filter(counts, country == "Finland")$riots), 0L)
})

# Warnings ----
test_that("Returns warning when start_date is before earliest date", {
   expect_warning(acled_generate_counts(data,
                                  unit_id = "country",
                                  time_id = "event_date",
                                  time_target = "month",
                                  start_date = "2021-12-01")
   )
})

test_that("Returns warning when end_date is after latest date", {
  expect_warning(acled_generate_counts(data,
                                  unit_id = "country",
                                  time_id = "event_date",
                                  time_target = "month",
                                  end_date = "2023-03-01")
  )
})
