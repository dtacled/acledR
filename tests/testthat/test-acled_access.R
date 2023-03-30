test_that("It shows that it works", {
  expect_message(acled_access(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5"), "Success! Credentials authorized")
})

test_that("It shows that is doesn't work", {
  expect_error(acled_access(email = "acledexamples@gmail123123.com", key = "M3PWwg3DIdhHMuDiilp5"), regex = "Key and email not authorized.*")
})

