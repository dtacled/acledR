#' @title Transform ACLED data from wide to long
#' @name acled_transform_longer
#' @description Function to convert your ACLED's API calls (if dyadic) into desired monadic forms.
#' @param data, dataframe or tibble containing your dataset.
#' @param type, character string. One of five types: full_actors, main_actors, assoc_actors, source, or all.
#' \itemize{
#' \item full_actors: All actor and associated actor columns
#' \item main_actors: Actor 1 and Actor 2 columns
#' \item assoc_actors: All associated actor columns
#' \item source: The source column becomes monadic
#' }
#' @return A tibble with the data transformed into long form.
#' @family Data Manipulation
#' @examples
#' \dontrun{
#' # argen_acled <- acled_api(country = "Argentina",start_date = "2022-01-01",
#' #                          end_date="2022-02-01", acled_access = T, prompt = F)
#'
#' # argen_acled_long_actors <- acled_transform_wide_to_long(argen_acled,
#' #                                            type = "full_actor") # Transforming the data
#'
#' # nrow(argen_acled_long_actors) # Number of rows in the dataset
#' # [1] 263 # Long form
#'
#' # nrow(argen_acled) ) # Number of rows in the dataset
#' # [1] 145 # Wide form
#' }
#' @md
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr relocate
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_trim
#'

acled_transform_longer <- function(data, type = "full_actors") {
  # To - do Remove NAs rows from the assoc actors.

  ## types: full_actors, main_actors,assoc_actors,source

  columns_present <- function(df, cols) {
    all(sapply(cols, function(x) !is.na(match(x, names(df)))))
  }




  if (type == "full_actors") { ## full actor -> pivot + separate into rows all actor columns

    if (!(columns_present(data, c("actor1", "actor2", "assoc_actor_1", "assoc_actor_2", "sub_event_type")))) {
      stop("Some columns are missing. Please make sure your data frame includes: actor1, actor2, assoc_actor_1, assoc_actor_2, sub_event_type, source_scale, source.")
    }

    if (any(grepl("[;]", data$actor1))) {
      stop("Your actor1 column seems to include more than one result per row. That is inconsistent with our column structure.")
    } else if (any(grepl("[;]", data$actor2))) {
      stop("Your actor2 column seems to include more than one result per row. That is inconsistent with our column structure.")
    }
    separated_data <- data %>%
      pivot_longer(cols = c("actor1", "actor2", "assoc_actor_1", "assoc_actor_2"), names_to = "type_of_actor", values_to = "actor") %>%
      separate_rows(actor, sep = ";") %>%
      # filter(actor != "") %>%
      relocate(c("type_of_actor", "actor"), .after = "sub_event_type") %>%
      mutate(actor = str_trim(actor)) %>%
      pivot_longer(cols = c("inter1", "inter2"), names_to = "inter_type", values_to = "inter") %>%
      filter(str_sub(type_of_actor, start = nchar(type_of_actor)) == str_sub(inter_type, start = nchar(inter_type))) %>%
      relocate(c("inter_type", "inter"), .after = "actor") %>%
      # Removing inters when the actor is an assoc_actor_1/2
      mutate(inter = case_when(
        str_detect(type_of_actor, "assoc_*") ~ NA,
        TRUE ~ inter
      ))

    if (0 %in% nchar(separated_data$actor)) {
      warning("There are empty rows in the actor column.")
    }
  } else if (type == "main_actors") { ## main_actors -> only pivot actor columns
    if (!(columns_present(data, c("actor1", "actor2", "assoc_actor_1", "assoc_actor_2", "sub_event_type")))) {
      stop("Some columns are missing. Please make sure your data frame includes: actor1, actor2, assoc_actor_1, assoc_actor_2, sub_event_type, source_scale, source.")
    }

    if (any(grepl("[;]", data$actor1))) {
      stop("Your actor1 column seems to include more than one result per row. That is inconsistent with our column structure.")
    } else if (any(grepl("[;]", data$actor2))) {
      stop("Your actor2 column seems to include more than one result per row. That is inconsistent with our column structure.")
    }
    separated_data <- data %>%
      pivot_longer(cols = c("actor1", "actor2"), names_to = "type_of_actor", values_to = "actor") %>%
      filter(actor != "") %>%
      relocate(c("type_of_actor", "actor"), .after = "sub_event_type") %>%
      mutate(actor = str_trim(actor)) %>%
      pivot_longer(cols = c("inter1", "inter2"), names_to = "inter_type", values_to = "inter") %>%
      filter(str_sub(type_of_actor, start = nchar(type_of_actor)) == str_sub(inter_type, start = nchar(inter_type))) %>%
      relocate(c("inter_type", "inter"), .after = "actor")
  } else if (type == "assoc_actors") { ## assoc_actors -> pivot + separate all assoc actor columns
    if (!(columns_present(data, c("actor1", "actor2", "assoc_actor_1", "assoc_actor_2", "sub_event_type")))) {
      stop("Some columns are missing. Please make sure your data frame includes: actor1, actor2, assoc_actor_1, assoc_actor_2, sub_event_type, source_scale, source.")
    }

    separated_data <- data %>%
      pivot_longer(cols = c("assoc_actor_1", "assoc_actor_2"), names_to = "type_of_assoc_actor", values_to = "assoc_actor") %>%
      separate_rows(assoc_actor, sep = ";") %>%
      relocate(c("type_of_assoc_actor", "assoc_actor"), .after = "sub_event_type") %>%
      mutate(assoc_actor = str_trim(assoc_actor))



    if (0 %in% nchar(separated_data$assoc_actor)) {
      warning("There are empty rows in the assoc_actor column.")
    }
  } else if (type == "source") { ## source -> pivot + separate source column
    if (!(columns_present(data, c("source_scale", "source")))) {
      stop("Some columns are missing. Please make sure your data frame includes: actor1, actor2, assoc_actor_1, assoc_actor_2, sub_event_type, source_scale, source.")
    }

    separated_data <- data %>%
      separate_rows(source, sep = ";") %>%
      mutate(source = str_trim(source, side = "both")) %>%
      relocate(source, .before = "source_scale") %>%
      mutate(source = str_trim(source))
  }

  return(separated_data)
}
