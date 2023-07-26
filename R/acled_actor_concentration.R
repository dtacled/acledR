#' @title Calculate actor concentration indices from ACLED data
#' @name acled_actor_concentration
#'
#' @param events Vector of outcomes per actor (i.e., event counts or fatalities), or ACLED data frame.
#' @param acled_dataframe Boolean. True or False statement on whether events is a dataframe. Defaults to True
#' @param method Index method. Default is "Effective actors", which is an Inverse Simpson Index. "Concentration" calculates a Herfindahl–Hirschman Index.
#' @return Returns a data.frame of the index value (`eff_actors` or `concentration`, depending on the method specified), number of unique actors, and average number of events per actor.
#'
#' @family Data Analysis
#'
#' @examples
#' \dontrun{
#' # These examples show the use of this function with simulated event counts from 5 actors
#' #library(acledR)
#'
#' # In the first case, each actor is attributed the same event count (10 events)
#' #df1 <- data.frame(actor = 1:5,
#' #               event_count = c(10, 10, 10, 10, 10))
#'
#' # Using the "Effective actors" method, 5 actors, each responsible for 10 events,
#' # returns the value of 5 for `eff_actors`
#' #acled_actor_concentration(df1$event_count, method = "Effective actors")
#'
#' #Using the "Concentration" method, the same data returns a value of 0.2 for `concentration`
#' #acled_actor_concentration(df1$event_count, method = "Concentration")
#'
#' # In the next case, 1 actor is responsible for 10 events, while the other 4
#' # are responsible for only 1 event each
#' #df2 <- data.frame(actor = 1:5,
#'               event_count = c(10, 1, 1, 1, 1))
#'
#' #The "Effective actors" method returns a value of 1.88 `eff_actors`
#' #acled_actor_concentration(df2$event_count, method = "Effective actors")
#'
#' #And the "Concentration" method returns a value of 0.53 for `concentration`
#' #acled_actor_concentration(df2$event_count, method = "Concentration")
#'
#' #Finally, when 2 actors are responsible for 10 events each and the other 3 actors only 1 event
#' #df3 <- data.frame(actor = 1:5,
#'               event_count = c(10, 10, 1, 1, 1)
#' #The "Effective actors" method returns a value of around 2.61 `eff_actors`
#' #acled_actor_concentration(df3$event_count, method = "Effective actors")
#'
#' #And the "Concentration" method returns a value of 0.38 for `concentration`
#' #acled_actor_concentration(df3$event_count, method = "Concentration")
#' }
#' @export
acled_actor_concentration <- function(events, method = "Effective actors", acled_dataframe = T){

  if(acled_dataframe == T){

      if(!is.data.frame(events)){
        stop("Events is not a dataframe. If you are using a vector, please utilize 'acled_dataframe = F'.")
        }
    suppressWarnings(
      suppressMessages(
        actors_dataset <- events %>%
          acled_transform_longer() %>%
          group_by(.data$actor) %>%
          summarise(n_events = n()) %>%
          filter(!is.na(actor))
      )
    )
      actors <- nrow(actors_dataset)
      avg_events <- mean(actors_dataset$n_events)

      events <- actors_dataset$n_events

      }else{
        if(!is.numeric(events)){
          stop("Events is not a vector of outcomes per actor. If it is an acled dataframe please select 'acled_dataframe = T'.")
          }
        actors <- length(events)
        avg_events <- mean(events)
        }

    if(method == "Effective actors") {
      eff_actors <- 1 / sum((events / sum(events)) ^ 2)
      return(data.frame(eff_actors = eff_actors, actors = actors, avg_events = avg_events))
    }
    if(method == "Concentration") {
      shares <- events / sum(events)
      concentration <- sum(shares ^ 2)
      return(data.frame(concentration = concentration, actors = actors, avg_events = avg_events))
    }
    else {
      stop("Method not 'Effective actors' or 'Concentration'.")
    }}

