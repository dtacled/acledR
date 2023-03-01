# Output type tests

test_that("It returns Inv Simpson Index", {
  expect_true(acled_actor_concentration(received_data_ac$event_number, method = "Effective actors", acled_dataframe = F)[["eff_actors"]] > 0.9 &
                  acled_actor_concentration(received_data_ac$event_number, method = "Effective actors", acled_dataframe = F)[["eff_actors"]] < (nrow(received_data_ac) + 1))
  })

test_that("It returns HHI", {
  expect_true(acled_actor_concentration(received_data_ac$event_number, method = "Concentration", acled_dataframe = F)[["concentration"]] > 0 &
                acled_actor_concentration(received_data_ac$event_number, method = "Concentration", acled_dataframe = F)[["concentration"]] < 1.01)
})

# Behavior tests

test_that("It stops unexpected object type when dataframe is true",{
  expect_error(acled_actor_concentration(received_data_ac$event_number, method = "Concentration", acled_dataframe = T), regexp = "Events is not a dataframe*")
})

test_that("It recognizes the proper object type when dataframe is true",{
  expect_error(acled_actor_concentration(received_data, method = "Concentration", acled_dataframe = T), NA)
})

test_that("It stops unexpected object type when dataframe is false",{
  expect_error(acled_actor_concentration(received_data, method = "Concentration", acled_dataframe = F), regexp = "Events is not a vector*")
})

test_that("It recognizes the proper object type when dataframe is false",{
  expect_error(acled_actor_concentration(received_data_ac$event_number, method = "Concentration", acled_dataframe = F), NA)
})
