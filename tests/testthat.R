if(identical(Sys.getenv("NOT_CRAN"), "true")) {

  library(testthat)
  library(acledR)

  test_check("acledR")
}
