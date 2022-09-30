#' @title Filter ACLED Data by Actor
#' @name filter_actors
#' @description This function allows users to filter ACLED data to a single actor or list of actors.
#' @param df dataframe. ACLED dataframe to filter.
#' @param actors character vector or vector of character strings. Actor name or list of actor names to use for filtering.
#' @param filter_cols character string. Columns to use for filtering. Default is "all", which filters based on all actor columns (i.e. actor1, actor2, assoc_actor_1, assoc_actor_2). "Primary" filters based on actor1 and actor2 column only.
#' @returns Returns a tibble of ACLED data filtered to selected actors.
#' @family Data Manipulation
#' @examples
#' \dontrun{
#'
#' }
#' @md
#' @import httr
#'
#' @export

filter_actors <- function(df, actors, filter_cols='all'){
  if(filter_cols=='all'){
    if(!'assoc_actor_1' %in% names(df) | !'assoc_actor_2' %in% names(df)){
      stop("Dataframe is missing 'assoc_actor_1' or 'assoc_actor_2' column.")
    }

    aa_regex <- str_replace_all(actors, '\\(', '\\\\(')
    aa_regex <- str_replace_all(aa_regex, '\\)', '\\\\)')
    aa_regex <- paste0('((^|; )', paste0(aa_regex, collapse = '($|;)|(^|; )'), '($|;))')

    filtered_df <- df %>%
      filter(actor1 %in% actors | actor2 %in% actors |
               str_detect(assoc_actor_1, aa_regex) | str_detect(assoc_actor_2, aa_regex))
  } else if (filter_cols=='primary') {
    filtered_df <- df %>%
      filter(actor1 %in% actors | actor2 %in% actors)
  } else {
    stop("filter_cols argument must be 'all' or 'primary'.")
  }

  return(filtered_df)
}
