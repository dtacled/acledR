#' @title Reverse Transform ACLED Data from Long to Wide
#' @name acled_transform_wider
#' @description Function to convert your ACLED's API calls (if monadic) back into the original dyadic forms.
#' @param data, a dataframe or tibble containing your dataset.
#' @param type, a character string. One of five types: full_actors, main_actors, assoc_actors, source, or all.
#' \itemize{
#' \item full_actors: All actor and associated actor columns
#' \item main_actors: Actor 1 and Actor 2 columns
#' \item assoc_actors: All associated actor columns
#' \item source: The source column becomes dyadic
#' \item api_monadic: Use this option for data that is the output of the API's monadic option.
#' }
#' @return A tibble with the data transformed back into wide form.
#' @family Data Manipulation
#' @examples
#' \dontrun{
#' # argen_acled <- acled_api(country = "Argentina",start_date = "2022-01-01",
#' #                          end_date="2022-02-01", acled_access = T, prompt = F)
#' # argen_acled_long_actors <- acled_transform_longer(argen_acled,
#' #                        type = "full_actor") # Transforming the data to long form
#'
#' # argen_acled_wide <- acled_transform_wider(argen_acled_long_actors,
#' #                        type = "full_actor") # Transforming the data back to wide form
#'
#' # nrow(argen_acled_wide) # Number of rows in the dataset
#' # [1] 145 # Wide form
#'
#' # nrow(argen_acled_long_actors) # Number of rows in the dataset
#' # [1] 263 # Long form
#' }
#' @md
#' @export
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom stringr str_c str_trim

acled_transform_wider <- function(data, type = "full_actors") {
  if (!(type %in% c("full_actors", "main_actors", "assoc_actors", "source", "api_monadic"))) {
    stop(paste0("Error: ", type, " is not a valid option. Please select a valid option:\"full_actors\", \"main_actors\", \"assoc_actors\", \"source\", \"api_monadic\""))
  }

  if(type %in% c("full_actors", "main_actors")) {
    inter_numeric <- any(1:8 %in% unique(data$inter))
  }

  if (type == "full_actors") {
    columns_present <- function(df, cols) {
      all(sapply(cols, function(x) !is.na(match(x, names(df)))))
    }

    colnames_long <- c(
      "actor", "type_of_actor", "inter_type", "inter"
    )
    if (!(columns_present(data, colnames_long))) {
      stop("Some columns are missing. Please make sure your data frame includes: actor,type_of_actor,inter_type, and inter.")
    }



    reverse_data <- data %>%
      # Pivot actor firsts, flattening joint actors such as assoc actors
      pivot_wider(names_from = type_of_actor, values_from = actor, values_fn = function(x) str_flatten(x, collapse = "; "), values_fill = "") %>%

      mutate(
        inter = as.character(inter)
      ) %>%

      # Pivot inters next, adding a fill 9999 to those that do not apply, as a way of removing. inters from different types of actors
      pivot_wider(names_from = inter_type, values_from = inter, values_fill = as.character(9999)) %>%
      mutate(inter1 = replace_na(inter1, "")) %>%
      mutate(inter2 = replace_na(inter2, "")) %>%
      group_by(across(c(-actor1, -actor2, -inter1, -inter2, -assoc_actor_1, -assoc_actor_2))) %>%
      # Collapse repeated inters and actors
      summarise(
        actor1 = str_c(actor1, collapse = ""),
        actor2 = str_c(actor2, collapse = ""),
        inter1 = str_trim(str_remove_all(str_c(inter1, collapse = " "), "9999|\\s0\\s")),
        inter2 = str_trim(str_remove_all(str_c(inter2, collapse = " "), "9999|\\s0\\s")),
        assoc_actor_1 = str_c(assoc_actor_1, collapse = ""),
        assoc_actor_2 = str_c(assoc_actor_2, collapse = "")
      ) %>%
      ungroup() %>%
      mutate(
        actor2 = na_if(actor2, ""),
        actor1 = na_if(actor1, ""),
        assoc_actor_1 = na_if(assoc_actor_1, ""),
        assoc_actor_2 = na_if(assoc_actor_2, "")
      ) %>%
      # Match column structure for an acled dataset
      dplyr::select(names(acledR::acled_old_dummy))

    # Coerce to numeric if inter were originally numeric
    if(inter_numeric == TRUE) {
      reverse_data <-
        reverse_data %>%
        mutate(
          inter1 = as.numeric(inter1),
          inter2 = as.numeric(inter2),
          inter1 = replace_na(inter1, 0),
          inter2 = replace_na(inter2, 0)
        )
    } else {
      reverse_data <-
        reverse_data %>%
        mutate(
          inter1 = case_when(inter1 == "" ~ NA_character_, TRUE ~ inter1),
          inter2 = case_when(inter2 == "" ~ NA_character_, TRUE ~ inter2)
        )
    }

  } else if (type == "main_actors") {
    columns_present <- function(df, cols) {
      all(sapply(cols, function(x) !is.na(match(x, names(df)))))
    }

    colnames_long <- c(
      "actor", "type_of_actor", "inter_type", "inter"
    )
    if (!(columns_present(data, colnames_long))) {
      stop("Some columns are missing. Please make sure your data frame includes: actor,type_of_actor,inter_type, and inter.")
    }

    reverse_data <- data %>%
      # Pivot actor firsts, flattening joint actors such as assoc actors
      pivot_wider(names_from = type_of_actor, values_from = actor, values_fn = function(x) str_flatten(x, collapse = "; "), values_fill = "") %>%

      # Transform inter into character for collapsing
      mutate(
        inter = as.character(inter)
      ) %>%

      # Pivot inters next, adding a fill 9999 to those that do not apply, as a way of removing. inters from different types of actors
      # Coerced to character to account for inters being text or numeric
      pivot_wider(names_from = inter_type, values_from = inter, values_fill = as.character(9999)) %>%

      mutate(inter1 = replace_na(inter1, "")) %>%
      mutate(inter2 = replace_na(inter2, "")) %>%
      group_by(across(c(-actor1, -actor2, -inter1, -inter2))) %>%
      # Collapse repeated inters and actors
      summarise(
        actor1 = str_c(actor1, collapse = ""),
        actor2 = str_c(actor2, collapse = ""),
        inter1 = str_trim(str_remove_all(str_c(inter1, collapse = " "), "9999")),
        inter2 = str_trim(str_remove_all(str_c(inter2, collapse = " "), "9999"))
      ) %>%
      ungroup() %>%
      mutate(
        actor2 = na_if(actor2, ""),
        actor1 = na_if(actor1, ""),
        assoc_actor_1 = na_if(assoc_actor_1, ""),
        assoc_actor_2 = na_if(assoc_actor_2, "")
      ) %>%
      # Match column structure for an acled dataset
      dplyr::select(names(acledR::acled_old_dummy))

    # Coerce to numeric if inter were originally numeric
    if(inter_numeric == TRUE) {
      reverse_data <-
        reverse_data %>%
        mutate(
        inter1 = as.numeric(inter1),
        inter2 = as.numeric(inter2),
        inter1 = replace_na(inter1, 0),
        inter2 = replace_na(inter2, 0)
      )
    } else {
      reverse_data <-
        reverse_data %>%
        mutate(
          inter1 = case_when(inter1 == "" ~ NA_character_, TRUE ~ inter1),
          inter2 = case_when(inter2 == "" ~ NA_character_, TRUE ~ inter2)
        )
    }

  } else if (type == "assoc_actors") {
    columns_present <- function(df, cols) {
      all(sapply(cols, function(x) !is.na(match(x, names(df)))))
    }

    colnames_long <- c(
      "assoc_actor", "type_of_assoc_actor"
    )
    if (!(columns_present(data, colnames_long))) {
      stop("Some columns are missing. Please make sure your data frame includes: assoc_actor,type_of_assoc_actor.")
    }

    reverse_data <- data %>%
      # Pivot actor firsts, flattening joint actors such as assoc actors
      pivot_wider(names_from = type_of_assoc_actor, values_from = assoc_actor, values_fn = function(x) str_flatten(x, collapse = "; "), values_fill = "") %>%
      # Transform inter into character for collapsing
      group_by(across(c(-assoc_actor_1, -assoc_actor_2))) %>%
      # Collapse repeated inters and actors
      summarise(
        assoc_actor_1 = str_c(assoc_actor_1, collapse = ""),
        assoc_actor_2 = str_c(assoc_actor_2, collapse = "")
      ) %>%
      ungroup() %>%
      mutate(
        actor2 = na_if(actor2, ""),
        actor1 = na_if(actor1, ""),
        assoc_actor_1 = na_if(assoc_actor_1, ""),
        assoc_actor_2 = na_if(assoc_actor_2, "")
      ) %>%
      # Match column structure for an acled dataset
      dplyr::select(names(acledR::acled_old_dummy))

    # Coerce to numeric if inter were originally numeric
    # if(inter_numeric == TRUE) {
    #   reverse_data <-
    #     reverse_data %>%
    #     mutate(
    #       inter1 = as.numeric(inter1),
    #       inter2 = as.numeric(inter2),
    #       inter1 = replace_na(inter1, 0),
    #       inter2 = replace_na(inter2, 0)
    #     )
    # } else {
    #   reverse_data <-
    #     reverse_data %>%
    #     mutate(
    #       inter1 = case_when(inter1 == "" ~ NA_character_, TRUE ~ inter1),
    #       inter2 = case_when(inter2 == "" ~ NA_character_, TRUE ~ inter2)
    #     )
    # }

  } else if (type == "source") {
    columns_present <- function(df, cols) {
      all(sapply(cols, function(x) !is.na(match(x, names(df)))))
    }

    colnames_long <- c(
      "source"
    )
    if (!(columns_present(data, colnames_long))) {
      stop("Some columns are missing. Please make sure your data frame includes: source")
    }

    reverse_data <- data %>%
      group_by(across(c(-source))) %>%
      # Collapse repeated inters and actors
      summarise(source = str_c(source, collapse = "; ")) %>%
      ungroup() %>%
      # Match column structure for an acled dataset
      dplyr::select(names(acledR::acled_old_dummy))
  } else if (type == "api_monadic") {
    df1 <- data %>%
      group_by(event_id_cnty) %>%
      slice(1) %>%
      ungroup() %>%
      rename(
        actor1 = actor1,
        assoc_actor_1 = assoc_actor_1
      )

    df2 <- data %>%
      group_by(event_id_cnty) %>%
      slice(2) %>%
      ungroup() %>%
      rename(
        actor2 = actor1,
        assoc_actor_2 = assoc_actor_1,
        inter2 = inter1
      )

    reverse_data <- df1 %>%
      left_join(df2, by = c(
        "event_id_cnty", "event_date", "year", "time_precision", "disorder_type", "event_type",
        "sub_event_type", "interaction", "civilian_targeting", "iso", "region", "country", "admin1",
        "admin2", "admin3", "location", "latitude", "longitude", "geo_precision", "source", "source_scale",
        "notes", "fatalities", "tags", "timestamp"
      )) %>%
      relocate(c(actor2, assoc_actor_2, inter2), .after = inter1) %>%
      mutate(inter2 = replace_na(inter2, 0)) %>%
      mutate(admin3 = as.logical(admin3)) %>%
      arrange(desc(event_id_cnty))
  }
  return(reverse_data)
}
