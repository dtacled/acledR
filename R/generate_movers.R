#' Generate moving statistics
#'
#' @param data ACLED data.
#' @param unit_id Unit variable.
#' @param time_id Temporal variable.
#' @param slide_funs Requested moving statistics. Character vector with options including mean, median, min, and max.
#' @param slide_periods How many periods in the past to summarize over. Vector of one or more integers. Inf includes all previous periods.
#' @param na.rm  Whether to include NAs in the calculations.
#' @return Returns a tibble grouped by unit_id.
#' @import dplyr
#' @import tidyr
#' @import slider
#' @import purrr
#'
#'
#' @export


generate_movers <-
  function(data, var, unit_id, time_id, slide_funs, slide_periods, na.rm = T, complete = T) {

    all_funs <- list(mean = function(x) {mean(x, na.rm = na.rm)},
                     sd = function(x) {sd(x, na.rm = na.rm)},
                     min = function(x) {min(x, na.rm = na.rm)},
                     max = function(x) {max(x, na.rm = na.rm)})

    cross_tbl <- expand_grid(all_funs, slide_periods) %>%
      mutate(fun_name = attr(all_funs, "name")) %>%
      filter(fun_name %in% slide_funs)


    data %>%
      group_by(.data[[unit_id]]) %>%
      arrange(.data[[unit_id]], .data[[time_id]]) %>%

      nest() %>%
      mutate(moving = map(data,
                          function(df) {
                            map2_dfc(.x = cross_tbl$all_funs,
                                     .y = cross_tbl$slide_periods,
                                     ~slider::slide_dbl(.x = df[[var]],
                                                        .f = .x, .before = .y,
                                                        .after = -1,
                                                        .complete = complete)) %>%
                              rename_with(., ~paste("moving", attr(cross_tbl$all_funs, "name"), cross_tbl$slide_periods, sep = "_"))
                          }
      )
      ) %>%

      unnest(c(data, moving)) %>%
      suppressMessages()

  }
