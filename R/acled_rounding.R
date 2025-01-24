#' @title Rounding function
#' @name acled_rounding
#' @description This function addresses some of the conflicts of rounding in R, especially when trying to round up.
#' @param num int. This is the number we are trying to round.
#' @param digits int. Where do we want to round up. It accepts 0 (whole number), 1 (tenth place), 2 (hundredths), etc.
#' @family Helpers
#' @returns A rounded numeric value
#' @details
#' This function is meant to address the problem of rounding in R where the approach is always round to even. The function is meant to round things following the simple rule. If the decimal is 5+ then round up, if not round down. With the 'digits' argument, one can set up the specificity of the rounding, 0= whole number, 1 = tenth place, 2=hundreds place, and so on.
#' @examples
#' x1 <- 1.569
#' x2 <- 104.530
#' x3 <- 54.430
#' x4 <- 205.49999
#' acled_rounding(x1)
#' acled_rounding(x2)
#' acled_rounding(x3)
#' acled_rounding(x4)
#' @md
#' @export





acled_rounding <- function(num, digits = 0) {
  num <- as.numeric(num)
  digits <- as.numeric(digits)

  accuracy <- 1 / (10^digits)
  extract_factor <- 10^(digits + 1)
  key_digit <- (trunc(num * extract_factor)) %% 10

  round_type <- ifelse(key_digit == 5,
    ceiling,
    round
  )

  round_any <- function(x, accuracy, f = round) {
    f(x / accuracy) * accuracy
  }

  return(round_any(num, accuracy, f = round_type))
}
