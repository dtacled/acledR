#' @title Change interaction codes from numeric labels to string labels
#' @name acled_transform_interaction
#' @description This function allows users to change from numeric interaction codes (i.e. 1, 2, 3, etc) to string interaction codes (i.e. State Forces, Rebel Group, etc.)
#' @param df dataframe. ACLED data including at least inter1, inter2 columns. If `only_inters` is TRUE, it also requires interaction column.
#' @param only_inters boolean. Option whether to include the *interaction* column in the transformation (if TRUE) or to only use *inter1* and *inter2* (if FALSE).
#' @returns Returns a tibble of of ACLED events with modified *inter1*, *inter2* and potentially *interaction* columns .
#' @family Data Manipulation
#' @examples
#' \dontrun{
#'
#' # Load data frame
#' argen_acled <- acled_api(
#'   email = "your_email", password = "your_password",
#'   country = "Argentina", start_date = "2022-01-01", end_date = "2022-02-01"
#' )
#'
#' # Transform the interactions
#' argen_acled_transformed <- acled_transformation_interaction(argen_acled, only_inters = F)
#' }
#' @md
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr relocate
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @export
#'


acled_transform_interaction <- function(df, only_inters = FALSE) {
  if (!"inter1" %in% colnames(df)) {
    stop("The input dataframe does not contain 'inter1' column. Please utilize a dataframe that has ACLED's column structure for the function to succeed.")
  }
  if (!"inter2" %in% colnames(df)) {
    stop("The input dataframe does not contain 'inter2' column. Please utilize a dataframe that has ACLED's column structure for the function to succeed.")
  }
  if (!"interaction" %in% colnames(df)) {
    stop("The input dataframe does not contain 'interaction' column. Please utilize a dataframe that has ACLED's column structure for the function to succeed.")
  }


  if (max(df$inter1 > 8) | max(df$inter2) > 8 | min(df$inter1 < 0 | min(df$inter2) < 0)) {
    stop("One or more interaction codes were not recognized. Please remember interaction codes are positive integers from 1 to 8. ")
  }



  test_changes <- df %>%
    left_join(acledR::acled_interaction_codes, by = c("inter1" = "Numeric Code")) %>%
    select(-inter1) %>%
    rename(inter1 = "Inter1/Inter2") %>%
    relocate(inter1, .after = assoc_actor_1) %>%
    left_join(acledR::acled_interaction_codes, by = c("inter2" = "Numeric Code")) %>%
    select(-inter2) %>%
    rename(inter2 = "Inter1/Inter2") %>%
    relocate(inter2, .after = assoc_actor_2)

  if (only_inters == F) {
    test_changes <- test_changes %>%
      mutate(interaction = case_when(
        str_detect(interaction, "10") ~ "Sole State Forces",
        str_detect(interaction, "20") ~ "Sole Rebel Groups",
        str_detect(interaction, "30") ~ "Sole Political Militias",
        str_detect(interaction, "40") ~ "Sole Identity Militias",
        str_detect(interaction, "50") ~ "Sole Rioters",
        str_detect(interaction, "60") ~ "Sole Protesters",
        str_detect(interaction, "70") ~ "Sole Civilians",
        str_detect(interaction, "80") ~ "Sole Others",
        TRUE ~ as.character(interaction)
      )) %>%
      mutate(
        interaction = str_replace_all(interaction, "(\\d)(\\d)", "\\1-\\2"),
        interaction = str_replace(as.character(interaction), "1", "State Forces"),
        interaction = str_replace(as.character(interaction), "2", "Rebel Groups"),
        interaction = str_replace(as.character(interaction), "3", "Political Militias"),
        interaction = str_replace(as.character(interaction), "4", "Identity Militias"),
        interaction = str_replace(as.character(interaction), "5", "Rioters"),
        interaction = str_replace(as.character(interaction), "6", "Protesters"),
        interaction = str_replace(as.character(interaction), "7", "Civilians"),
        interaction = str_replace(as.character(interaction), "8", "External/Other Forces")
      )
  }


  return(test_changes)
}
