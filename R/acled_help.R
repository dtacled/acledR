#' @title ACLED's dataset help
#' @name acled_help
#' @description The function allows users to consult methodological vignettes for the different columns in ACLED's dataset
#' @param column string. A string with the name of the column for which to consult methodology.
#' @returns An acledR vignette
#' @seealso
#' \itemize{
#' \item ACLED resources and methodology guides <https://acleddata.com/resources/>
#' }
#' @examples
#' \dontrun{
#'
#' ## Get information about the fatalities column
#' acled_help("fatalities")
#' }
#' @md
#' @export

# Building the function

acled_help <- function(column = NULL){

  if(is.null(column)){ # If no column is indicated
    message("You have not indicated any column. If you want to consult all our methodology documentation, see https://acleddata.com/resources/#methodology")
    vignette("acled_codebook", package="acledR")

  } else if(column == "iso"|column == "event_id_cnty"|column == "event_id_no_cnty"){ # ISO, event_id_cnty,event_id_no_cnty
    vignette("acled_deletions_api", package="acledR")

  } else if(column == "event_date" | column == "year" | column == "time_precision"){ # event_date, year, time_precision
    vignette("time",package="acledR")
  } else if(column == "event_type" | column == "sub_event_type"){  # event_type,sub_event_type
    vignette("event_types", package="acledR")
  } else if(column == "actor1" | column == "assoc_actor_1" | column == "inter1" | column == "actor2" | column == "assoc_actor_2" | column == "inter2" | column == "interaction"){  # Actor1, assoc_actor 1, inter1,actor2,assoc_actor_2,inter2, interaction
    vignette("actors_interactions", package="acledR")
  } else if(column == "region" | column == "country"|column == "admin1"|column == "admin2"|column == "admin3"|column == "location"|column == "latitude"|column == "longitude"|column == "geo_precision"){  # region,country,admin1,admin2,admin3,location,latitude,longitude,geo_precision
    vignette("geography", package="acledR")
  } else if(column == "source"|column == "source_scale"){  # source,source_scale
    vignette("acled_codebook", package="acledR")
  } else if(column == "notes"){ # notes
    vignette("acled_codebook", package="acledR")
  } else if(column == "fatalities"){   # fatalities
    vignette("fatalities",package="acledR")
  } else if(column == "timestamp"){  # timestamp
    vignette("acled_deletions_api", package="acledR")
  }
}

