#' ACLED Codebook
#'
#' Codebook for ACLED data
#'
#' @family Data
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
#' @family Data
#' @format A data frame:
#' \describe{
#' \item{country}{Country names}
#' \item{region}{Region names}
#' \item{start_year}{First year coded by ACLED}}
"acled_countries"

#' ACLED Multipliers
#'
#' A dataframe with additional information for each country, only for the purpose of estimating events.
#'
#' @family Data
#' @format A data frame:
#' \describe{
#' \item{country}{Country names}
#' \item{bin}{Bin of event frequency}
#' \item{year}{Year corresponding to the bin}
#' \item{avg_month_bin}{Average monthly of the bin}
#' }
"acled_multipliers"

#' ACLED Regions
#'
#' ACLED region names, region numbers, and coding start dates
#'
#' @family Data
#' @format A data frame:
#' \describe{
#' \item{region}{Region number}
#' \item{region_name}{Region names}
#' \item{first_event_date}{First date (yyyy-mm-dd) coded by ACLED}}
"acled_regions"

#' ACLED Event Categories
#'
#' ACLED event and sub-event types, grouped by category
#'
#' @family Data
#' @format A data frame:
#' \describe{
#' \item{event_type}{ACLED event type}
#' \item{sub_event_type}{ACLED sub-event type}
#' \item{political_violence}{Dummy indicator for whether sub-event type falls within political violence}
#' \item{organized_political_violence}{Dummy indicator for whether sub-event type falls within organized political violence}
#' \item{disorder}{Dummy indicator for whether sub-event type falls within disorder}
#' \item{demonstrations}{Dummy indicator for whether sub-event type falls within demonstrations}}
"acled_event_categories"

#' A dummy data frame of ACLED events emulating an old format, used in "Keeping your dataset updated" Vignette
#'
#' Small dataset of events in Argentina, purposefully including events which are currently deleted/modified.
#' @family Data
#' @format A data frame:
#' \describe{
#' \item{event_id_cnty}{An unique individual identifier by number and country acronym (updated annually)}
#' \item{event_date}{The day, month and year on which an event took place}
#' \item{year}{The year in which an event took place}
#' \item{time_precision}{A numeric code indicating the level of certainty of the date coded for the event}
#' \item{disorder_type}{Type of disorder associated with the event and sub event type}
#' \item{event_type}{The type of event}
#' \item{sub_event_type}{The type of sub-event}
#' \item{actor1}{The named actor involved in the event. Note: Actor 1 and Actor 2 do not imply directionality (e.g. attacker or defender)}
#' \item{assoc_actor_1}{The named actor associated with or identifying actor1}
#' \item{inter1}{A numeric code indicating the type of actor1}
#' \item{actor2}{The named actor involved in the event. Note: Actor 1 and Actor 2 do not imply directionality (e.g. attacker or defender)}
#' \item{assoc_actor_2}{The named actor associated with or identifying actor1}
#' \item{inter2}{A numeric code indicating the type of actor1}
#' \item{interaction}{A numeric code indicating the interaction between types of actor1 and actor2}
#' \item{civilian_targeting}{Column referencing the presence of civilian targeting}
#' \item{iso}{A numeric code for each individual country}
#' \item{region}{The region of the world where the event took place}
#' \item{country}{The country in which the event took place}
#' \item{admin1}{The largest sub-national administrative region in which the event took place}
#' \item{admin2}{The second largest sub-national administrative region in which the event took place}
#' \item{admin3}{The third largest sub-national administrative region in which the event took place}
#' \item{location}{The location in which the event took place}
#' \item{latitude}{The latitude of the location}
#' \item{longitude}{The longitude of the location}
#' \item{geo_precision}{A numeric code indicating the level of certainty of the location coded for the event}
#' \item{source}{The source of the event report}
#' \item{source_scale}{The scale (local, regional, national, international) of the source}
#' \item{notes}{A short description of the event}
#' \item{fatalities}{The number of reported fatalities which occurred during the event}
#' \item{tags}{Tags associated with the event.}
#' \item{timestamp}{Numeric code of time}}
"acled_old_dummy"

#' Second dummy data frame of ACLED events emulating an old format, used in acled_deletion_api Vignette
#'
#' Large dataset of multiple regions and countries, purposefully including deleted/modified events.
#'
#' @family Data
#' @format A data frame:
#' \describe{
#' \item{event_id_cnty}{An unique individual identifier by number and country acronym (updated annually)}
#' \item{event_date}{The day, month and year on which an event took place}
#' \item{year}{The year in which an event took place}
#' \item{time_precision}{A numeric code indicating the level of certainty of the date coded for the event}
#' \item{disorder_type}{Type of disorder associated with the event and sub event type}
#' \item{event_type}{The type of event}
#' \item{sub_event_type}{The type of sub-event}
#' \item{actor1}{The named actor involved in the event. Note: Actor 1 and Actor 2 do not imply directionality (e.g. attacker or defender)}
#' \item{assoc_actor_1}{The named actor associated with or identifying actor1}
#' \item{inter1}{A numeric code indicating the type of actor1}
#' \item{actor2}{The named actor involved in the event. Note: Actor 1 and Actor 2 do not imply directionality (e.g. attacker or defender)}
#' \item{assoc_actor_2}{The named actor associated with or identifying actor1}
#' \item{inter2}{A numeric code indicating the type of actor1}
#' \item{interaction}{A numeric code indicating the interaction between types of actor1 and actor2}
#' \item{civilian_targeting}{Column referencing the presence of civilian targeting}
#' \item{iso}{A numeric code for each individual country}
#' \item{region}{The region of the world where the event took place}
#' \item{country}{The country in which the event took place}
#' \item{admin1}{The largest sub-national administrative region in which the event took place}
#' \item{admin2}{The second largest sub-national administrative region in which the event took place}
#' \item{admin3}{The third largest sub-national administrative region in which the event took place}
#' \item{location}{The location in which the event took place}
#' \item{latitude}{The latitude of the location}
#' \item{longitude}{The longitude of the location}
#' \item{geo_precision}{A numeric code indicating the level of certainty of the location coded for the event}
#' \item{source}{The source of the event report}
#' \item{source_scale}{The scale (local, regional, national, international) of the source}
#' \item{notes}{A short description of the event}
#' \item{fatalities}{The number of reported fatalities which occurred during the event}
#' \item{tags}{Tags associated with the event.}
#' \item{timestamp}{Numeric code of time}}
"acled_old_deletion_dummy"

#' ACLED interaction codes
#'
#' ACLED interaction and actor types
#'
#' @family Data
#' @format A data frame:
#' \describe{
#' \item{Inter1/Inter2}{Actor type}
#' \item{Numeric Code}{Numeric equivalent found in the inter1 and inter2 column. }}
"acled_interaction_codes"
