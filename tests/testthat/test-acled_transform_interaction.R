# Check that it produces the same result as intended


test_that("The function swaps the interaction codes properly",{

  expect_equal(acled_transform_interaction(test), test_changes)


})
