# Test for acled_deletions_api

# Basic check ----

## Received data ----
test_that("number of columns is correct - date", {
  skip_on_cran()
  expect_equal(ncol(received_deleted_data_date),2)
})

test_that("number of columns is correct - unix", {
  skip_on_cran()
  expect_equal(ncol(received_deleted_data_unix),2)
})

test_that("names of columns are correct - date", {
  skip_on_cran()
  expect_equal(names(received_deleted_data_date),columns_deleted)
})

test_that("names of columns are correct - unix", {
  skip_on_cran()
  expect_equal(names(received_deleted_data_unix),columns_deleted)
})

## Test that email and key are handled appropiately ----

test_that("Email and key are handled as expected without acled_access",{
  skip_on_cran()
  expect_true(grepl("acledrexamples", received_deleted_log$email[1]))}
)

## Test that acled_access is handled appropiately. ----

test_that("Email and Key are handled appropiately",{
  skip_on_cran()

  acled_access(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"))

  some_log <- acled_deletions_api(date_deleted = "1658707200", acled_access = T, log = T)

  expect_true(grepl("acledrexamples", some_log$email[1]))}
)

## Date and unix return the same output ----
test_that("names of columns are correct - unix", {
  skip_on_cran()

  expect_true(all.equal(received_deleted_data_date, received_deleted_data_unix))
})




# Errors

## If users do not set a timestamp, they will get all the deleted events in the dataset. ----

test_that("users can opt not to include a timestamp and they will get all the possible events", {
  skip_on_cran()

  stuff <- acled_deletions_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"), acled_access = F, log = F)

  expect_gt(nrow(stuff), 140150)
})

# Errors----
## Errors from badly written credentials ----

test_that("Users get an error if the email or key are not provided", {
  skip_on_cran()

  expect_error(acled_deletions_api(date_deleted = "1658707200", acled_access = F, log = T), regexp = "Email address required for ACLED API access")

  expect_error(acled_deletions_api(key = "key",
                                   date_deleted = "1658707200", acled_access = F, log = T), regexp = "Email address required for ACLED API access")

  expect_error(acled_deletions_api(email = "Email",
                                   date_deleted = "1658707200", acled_access = F, log = T), regexp = 'Key required for ACLED API access' )

})



## Test errors from badly utilized acled_access and key/email combination ----
test_that("If access is TRUE and credentials are null, credentials are ignored, but an error appears if Keys are empty in the enviornemt", {
  skip_on_cran()

  expect_error(
    Sys.setenv("acled_key" = "") %>%
      acled_deletions_api(acled_access = T,log = F), regexp = "acled_access is TRUE, but email and/or key are not stored in the enviornment. Please rerun acled_access or include key and email in function")
})



## A message appears that acled_access is being ignored, and the proper credentials are being used.  ----

test_that("A warning appears that acled_access is being ignored, and the proper credentials are being used.",{
  skip_on_cran()

  alog <- acled_deletions_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                  date_deleted = "1658707200",acled_access = T, log = T)

  expect_message(acled_deletions_api(email = Sys.getenv("EMAIL_ADDRESS_EXAMPLES"), key = Sys.getenv("EXAMPLES_KEY"),
                                     date_deleted = "1658707200",acled_access = T, log = T), regexp = "acled_access is TRUE, but email and key are included in the function. Ignoring acled_access.")

  expect_true(grepl("acledrexamples@gmail.com", alog$email[1]))
 })



