#' @title Transform ACLED data from wide to long
#' @name acled_transform
#' @description Function to convert your ACLED's API calls (if dyadic) into desired monadic forms.
#' @param data, Dataframe or tibble containing your dataset.
#' @param type, character string. One of five types: full_actors, main_actors, assoc_actors, source, or all.
#' \itemize{
#' \item full_actors: All actor and associated actor columns
#' \item main_actors: Actor 1 and Actor 2 columns
#' \item assoc_actors: All associated actor columns
#' \item source: The source column becomes monadic
#' \item all: All actor, associated actor and source columns become monadic
#' }
#' @return A tibble with the data transformed into long form.
#' @family Data Manipulation
#' @examples
#' \dontrun{
#' argen_acled <- acled_api(countries = "Argentina",start_date = "2022-01-01",
#'                          end_date="2022-02-01", acled_access = T, prompt = F)
#'
#' argen_acled_long_actors <- acled_transform(argen_acled,
#'                                            type = "full_actor") # Transforming the data
#'
#' nrow(argen_acled_long_actors) # Number of rows in the dataset
#' [1] 263 # Long form
#'
#' nrow(argen_acled) ) # Number of rows in the dataset
#' [1] 145 # Wide form
#' }
#' @md
#' @export
#' @importFrom rlang .data




acled_transform <- function(data,type="full_actors"){ ## types: full_actors, main_actors,assoc_actors,source, all

  if(type == "full_actors") { ## full actor -> pivot + separate into rows all actor columns
    separated_data <- data %>%
      pivot_longer(cols = c("actor1","actor2","assoc_actor_1","assoc_actor_2"),names_to = "type_of_actor",values_to = "actor") %>%
      separate_rows(.data$actor, sep = ";") %>%
      filter(.data$actor != "") %>%
      relocate(c("type_of_actor","actor"),.after="sub_event_type")%>%
      mutate(actor = str_trim(.data$actor))
  }else if (type == "main_actors"){ ## main_actors -> only pivot actor columns
    separated_data <- data %>%
      pivot_longer(cols = c("actor1","actor2"),names_to = "type_of_actor",values_to = "actor") %>%
      filter(.data$actor != "") %>%
      relocate(c("type_of_actor","actor"),.after="sub_event_type")%>%
      mutate(actor = str_trim(.data$actor))
  }else if (type == "assoc_actors"){ ## assoc_actors -> pivot + separate all assoc actor columns
    separated_data <- data %>%
      pivot_longer(cols = c("assoc_actor_1","assoc_actor_2"),names_to = "type_of_assoc_actor",values_to = "assoc_actor") %>%
      separate_rows(.data$assoc_actor, sep = ";") %>%
      relocate(c("type_of_assoc_actor","assoc_actor"),.after="sub_event_type")%>%
      mutate(assoc_actor = str_trim(.data$assoc_actor))
  }else if(type == "source"){  ## source -> pivot + separate source column
    separated_data <- data %>%
      separate_rows(.data$source, sep = ";") %>%
      mutate(source = str_trim(.data$source,side = "both")) %>%
      relocate(source,.before="source_scale")%>%
      mutate(source = str_trim(.data$source))
  }else if(type == "all"){ ## all -> pivot + separate all actor, assoc actor and source columns
    separated_data <- data %>%
      pivot_longer(cols = c("actor1","actor2","assoc_actor_1","assoc_actor_2"),names_to = "type_of_actor",values_to = "actor") %>%
      separate_rows(c("actor","source"), sep = ";") %>%
      filter(.data$actor != "") %>%
      relocate(c("type_of_actor","actor"),.after="sub_event_type") %>%
      relocate(.data$source, .before = "source_scale")%>%
      mutate(actor = str_trim(.data$actor))%>%
      mutate(assoc_actor = str_trim(.data$assoc_actor))
  }
}
