#' Calculate actor concentration indices from ACLED data
#'
#' @param events Vector of event counts per actor.
#' @param method Index method. Default is "Effective actors", which is a Inverse Simpson Index. "Concentration" calculates a Herfindahl–Hirschman Index.
#' @return Returns a tibble of the index value, number of unique actors, and average number of events per actor.
#'
#'
#'
#' @export

calculate_actor_index <-
  function(events, method = "Effective actors") {

    actors <- length(events)
    avg_events <- mean(events)

    if(method == "Effective actors") {
      eff_actors <- 1 / sum((events / sum(events)) ^ 2)
      return(tibble(eff_actors = eff_actors, actors = actors, avg_events = avg_events))
    }
    if(method == "Concentration") {
      shares <- events / sum(events)
      concentration <- sum(shares ^ 2)
      return(tibble(concentration = concentration, actors = actors, avg_events = avg_events))
    }
    else {
      stop("Method not 'Effective actors' or 'Concentration'.")
    }


  }
