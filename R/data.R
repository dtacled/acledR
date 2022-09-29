#' ACLED Codebook
#'
#' Codebook for ACLED data
#'
#' @format A data frame:
#' \describe{
#' \item{Variable}{Variable names}
#' \item{Description}{Text description of each variable}
#' \item{Values}{Text description of values for each variable}}
"acled_codebook"

#' ACLED Countries
#'
#' ACLED country names, regions, and coding start year
#'
#' @format A data frame:
#' \describe{
#' \item{country}{Country names}
#' \item{region_name}{Region names}
#' \item{start_year}{First year coded by ACLED}}
"acled_countries"

#' ACLED Regions
#'
#' ACLED region names, region numbers, and coding start dates
#'
#' @format A data frame:
#' \describe{
#' \item{region}{Region number}
#' \item{region_name}{Region names}
#' \item{first_event_date}{First date (yyyy-mm-dd) coded by ACLED}}
"acled_regions"
