#' @title Reverse Transform ACLED Data from Long to Wide
#' @name acled_reverse_transform
#' @description Function to convert your ACLED's API calls (if monadic) back into the original dyadic forms.
#' @param data, a dataframe or tibble containing your dataset.
#' @param type, a character string. One of five types: full_actors, main_actors, assoc_actors, source, or all.
#' \itemize{
#' \item full_actors: All actor and associated actor columns
#' \item main_actors: Actor 1 and Actor 2 columns
#' \item assoc_actors: All associated actor columns
#' \item source: The source column becomes dyadic
#' }
#' @return A tibble with the data transformed back into wide form.
#' @family Data Manipulation
#' @examples
#' \dontrun{
#' #argen_acled <- acled_api(countries = "Argentina",start_date = "2022-01-01",
#' #                          end_date="2022-02-01", acled_access = T, prompt = F)
#' #argen_acled_long_actors <- acled_transform(argen_acled,
#' #                                            type = "full_actor") # Transforming the data to long form
#'
#' #argen_acled_wide <- acled_reverse_transform(argen_acled_long_actors,
#' #                                            type = "full_actor") # Transforming the data back to wide form
#'
#' #nrow(argen_acled_wide) # Number of rows in the dataset
#' #[1] 145 # Wide form
#'
#' #nrow(argen_acled_long_actors) # Number of rows in the dataset
#' #[1] 263 # Long form
#' }
#' @md
#' @export
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_c str_trim

acled_reverse_transform <- function(data, type = "full_actors") {

  # Check if structure is the same as the acled_transform_wide_to_long() output

  if(type == "full_actors" || type == "main_actors") {

    columns_present <- function(df, cols) {
      all(sapply(cols, function(x) !is.na(match(x, names(df)))))}

    colnames_long <- c(
      "actor","type_of_actor","inter_type", "inter"
    )
    if(!(columns_present(data,colnames_long))){
      stop("Some columns are missing. Please make sure your data frame includes: actor,type_of_actor,inter_type, and inter.")
    }

    reverse_data <- data %>%
      # mutate(actor = str_c(actor, collapse = ";")) %>%
      pivot_wider(names_from = type_of_actor, values_from = actor, values_fn = list(actor = str_c), values_fill = "") %>%
      mutate(actor1 = str_trim(actor1),
             actor2 = str_trim(actor2),
             assoc_actor_1 = str_trim(assoc_actor_1),
             assoc_actor_2 = str_trim(assoc_actor_2))
  } else if(type == "assoc_actors") {
    reverse_data <- data %>%
      mutate(assoc_actor = str_c(assoc_actor, collapse = ";")) %>%
      pivot_wider(names_from = type_of_assoc_actor, values_from = assoc_actor, values_fn = list(assoc_actor = str_c), values_fill = "") %>%
      mutate(assoc_actor_1 = str_trim(assoc_actor_1),
             assoc_actor_2 = str_trim(assoc_actor_2))
  } else if(type == "source") {
    reverse_data <- data %>%
      mutate(source = str_c(source, collapse = ";")) %>%
      mutate(source = str_trim(source))
  }

  return(reverse_data)
}
