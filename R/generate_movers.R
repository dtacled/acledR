#' Generate moving statistics
#'
#' @param data ACLED data.
#' @param var Variable of interest.
#' @param unit_id Spatial unit of analysis (e.g., country, ADMIN1, region).
#' @param time_id Temporal variable (e.g., week, month, year).
#' @param slide_funs Requested moving statistics. Character vector with options including mean, median, sd, min, and max.
#' @param slide_periods How many periods in the past to summarize over. Vector of one or more integers. Inf includes all previous periods.
#' @param na.rm  Whether to include NAs in the calculations.
#' @param complete Whether to reqiure at least the requested time horizon to lapse prior to calculating moving statistics. For example, if slide_periods = 4, this would return NAs for the first 4 periods in the data and only begin calculating the moving statistic in the fifth time period. If FALSE, moving statistics for all available time periods are calculated, even if the requested time horizon has not elapsed.
#'
#' @return Returns a tibble grouped by unit_id.
#'
#' @family Data Analysis
#'
#' @examples
#' \dontrun{
#'
#' # Request all events in India since 2018
#' df_india <- acled_api(countries = "India",
#'                       start_date = "2018-01-01",
#'                       end_date = "2022-08-30",
#'                       monadic = F)
#'
#' # Aggregate to event counts per month across India
#' df_india_agg <-
#'    df_india %>%
#'    generate_counts(.,
#'    unit_id = "country",
#'    time_id = "event_date",
#'    time_target = "month")
#'
#' # Generate 3 moving average of total events per month
#' df_india_agg_movers <-
#'    generate_movers(data = df_india_agg,
#'    var = "total_events",
#'    unit_id = "country",
#'    time_id = "event_month",
#'    slide_funs = "mean",
#'    slide_periods = 3)
#'}
#'
#' @import dplyr
#' @import tidyr
#' @importFrom slider slide_dbl
#' @importFrom purrr map map2_dfc
#' @importFrom rlang .data
#' @importFrom stats sd median
#'
#' @export


generate_movers <-
  function(data, var, unit_id, time_id, slide_funs, slide_periods, na.rm = T, complete = T) {

    all_funs <- list(mean = function(x) {mean(x, na.rm = na.rm)},
                     sd = function(x) {sd(x, na.rm = na.rm)},
                     median = function(x) {sd(x, na.rm = na.rm)},
                     min = function(x) {min(x, na.rm = na.rm)},
                     max = function(x) {max(x, na.rm = na.rm)})

    cross_tbl <- expand_grid(all_funs, slide_periods) %>%
      mutate(fun_name = attr(all_funs, "name")) %>%
      filter(.data$fun_name %in% slide_funs)


    data %>%
      group_by(.data[[unit_id]]) %>%
      arrange(.data[[unit_id]], .data[[time_id]]) %>%

      nest() %>%
      mutate(moving = map(data,
                          function(df) {
                            map2_dfc(.x = cross_tbl$all_funs,
                                     .y = cross_tbl$slide_periods,
                                     ~slide_dbl(.x = df[[var]],
                                                .f = .x, .before = .y,
                                                .after = -1,
                                                .complete = complete)) %>%
                              rename_with(~paste(var, "moving",
                                                    attr(cross_tbl$all_funs, "name"),
                                                    cross_tbl$slide_periods, sep = "_"))
                          }
      )
      ) %>%

      unnest(c(data, .data$moving)) %>%
      suppressMessages()

  }
