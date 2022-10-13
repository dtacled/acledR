#' @title Filter ACLED Data by Actor
#' @name filter_actors
#' @description This function allows users to filter ACLED data by actor.
#' @param df dataframe. ACLED data to filter.
#' @param actors character vector or vector of character strings. Actor name or list of actor names to use for filtering.
#' @param filter_cols character string. Columns to use for filtering. Default is "all", which filters based on all actor columns (i.e. actor1, actor2, assoc_actor_1, assoc_actor_2). "Primary" filters based on actor1 and actor2 column only.
#' @returns Returns a tibble of ACLED data filtered to selected actors.
#' @family Data Manipulation
#' @examples
#' \dontrun{
#'
#' # Request events from Yemen
#' df_events <- acled_api(countries = "Yemen",
#'                        start_date = "2022-01-01",
#'                        end_date = "2022-07-30",
#'                        monadic = F,
#'                        acled_access = TRUE)
#'
#' # Create vector of actors of interest
#' yemen_actors <- c('Military Forces of Yemen (2016-) Supreme Political Council', 'Military Forces of Yemen (2012-)')
#'
#' # Filter to selected actors
#' filtered_df <- filter_actors(df = df_events, actors = yemen_actors, filter_cols = 'all')
#' }
#' @md
#' @import dplyr
#' @import stringr
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
