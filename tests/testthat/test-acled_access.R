skip_on_cran()


# Does it save the credentials in the enviornment? - Missing

test_that("acled_access properly stores the credentials", {
  skip_on_cran()

  expect_equal(Sys.getenv("acled_email"),"acledrexamples@gmail.com")
  expect_equal(nchar(Sys.getenv("acled_key")), 20)
})


# Shows the message that it was successful?
test_that("It shows that it works", {
  skip_on_cran()
  expect_message(acled_access(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY")), "Success! Credentials authorized")
})


# Does it shows a message when it fails?
test_that("It shows that is doesn't work", {
  skip_on_cran()
  expect_error(acled_access(email = "an@email!!!", key = "akey!!!"),
               regex = "Key and email not authorized.*")
})
