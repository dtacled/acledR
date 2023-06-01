
# Does it save the credentials in the enviornment? - Missing


# Shows the message that it was successful?
test_that("It shows that it works", {
  expect_message(acled_access(email = "acledexamples@gmail.com", key = "M3PWwg3DIdhHMuDiilp5"), "Success! Credentials authorized")
})


# Does it shows a message when it fails?
test_that("It shows that is doesn't work", {
  expect_error(acled_access(email = "acledexamples@gmail123123.com", key = "M3PWwg3DIdhHMuDiilp5"),
               regex = "Key and email not authorized.*")
})
