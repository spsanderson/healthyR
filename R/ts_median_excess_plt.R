#' Create a plot showing the excess of the median value
#'
#' @description
#' Plot out the excess +/- of the median value grouped by certain time parameters
#'
#' @param .data The data that is being analyzed, data must be a tibble/data.frame
#' @param .date_col The column of the tibble that holds the date
#' @param .value_col The column that holds the value of interest
#' @param .x_axis What is the be the x-axis, day, week, etc.
#' @param .ggplot_group_var The variable to group the ggplot on
#' @param .years_back How many yeas back do you want to go in order to compute
#' the median value
#'
#' @details
#' - Supply data that you want to view and you will see the excess +/- of the median values
#'   over a specified time series tibble
#'
#' @examples
#'
#' suppressPackageStartupMessages(library(timetk))
#'
#' ts_ymwdh_tbl(
#'   .data = m4_daily
#'   , .date_col = date
#' ) %>%
#' ts_median_excess_plt(
#'   .date_col = date
#'   , .value_col = value
#'   , .x_axis = wk
#'   , .ggplot_group_var = yr
#'   , .years_back = 5
#' )
#'
#' @return
#' A `ggplot2` plot
#'
#' @export
#'

ts_median_excess_plt <- function(
    .data
    , .date_col
    , .value_col
    , .x_axis
    , .ggplot_group_var
    , .years_back
) {

    # Tidayeval Setup
    date_var_expr         <- rlang::enquo(.date_col)

    value_var_expr        <- rlang::enquo(.value_col)
    value_var_name        <- rlang::quo_name(value_var_expr)

    x_axis_var_expr       <- rlang::enquo(.x_axis)
    x_axis_var_name       <- rlang::quo_name(x_axis_var_expr)

    ggplot_group_expr     <- rlang::enquo(.ggplot_group_var)

    years_back_expr       <- rlang::enquo(.years_back)

    # Checks
    if(!is.data.frame(.data)) {
        stop(call. = FALSE, "(data) is not a data-frame or tibble. Please supply.")
    }

    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "(date_var_expr) is missing. Please supply.")
    }

    if (rlang::quo_is_missing(value_var_expr)) {
        stop(call. = FALSE, "(value_var_expr) is missing. Please supply.")
    }

    if (rlang::quo_is_missing(x_axis_var_expr)) {
        stop(call. = FALSE, "(x_axis_var_expr) is missing. Please supply.")
    }

    if (rlang::quo_is_missing(years_back_expr)) {
        stop(call. = FALSE, "(years_back_expr) is missing. Please supply.")
    }

    # Get .end_date
    .end_date   <- .data %>%
        dplyr::select({{date_var_expr}}) %>%
        dplyr::pull({{date_var_expr}}) %>%
        base::max()

    # Data Manip
    df_grp_tbl <- tibble::as_tibble(.data) %>%
        dplyr::filter(lubridate::year({{date_var_expr}}) >= lubridate::year(.end_date) - {{years_back_expr}}) %>%
        dplyr::filter(lubridate::year({{date_var_expr}}) <= lubridate::year(.end_date) - 1) %>%
        dplyr::select(- {{date_var_expr}} ) %>%
        dplyr::group_by( {{ggplot_group_expr}}, {{x_axis_var_expr}} ) %>%
        dplyr::summarise(value = sum({{value_var_expr}})) %>%
        dplyr::ungroup() %>%
        dplyr::group_by({{x_axis_var_expr}}) %>%
        dplyr::summarise(median_value = stats::median(value)) %>%
        dplyr::ungroup()

    df_excess_tbl <- tibble::as_tibble(.data) %>%
        dplyr::select(- {{date_var_expr}} ) %>%
        dplyr::group_by( {{ggplot_group_expr}}, {{x_axis_var_expr}} ) %>%
        dplyr::summarise(value = sum( {{value_var_expr}} )) %>%
        dplyr::ungroup() %>%
        dplyr::group_by( {{x_axis_var_expr}} ) %>%
        dplyr::left_join(df_grp_tbl) %>%
        dplyr::mutate(excess = value - median_value) %>%
        dplyr::ungroup() %>%
        dplyr::select(-value, -median_value)

    g <- df_excess_tbl %>%
        dplyr::mutate(last_flag = (df_excess_tbl[[1]] == max(df_excess_tbl[[1]]))) %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = df_excess_tbl[[2]]
                , y = excess
                , group = df_excess_tbl[[1]]
            )
        ) +
        ggplot2::geom_hline(yintercept = 0, col='gray') +
        ggplot2::geom_line(ggplot2::aes(col=last_flag, y = excess)) +
        ggplot2::scale_color_manual(values = c("FALSE"='gray',"TRUE"='red')) +
        ggplot2::guides(col = FALSE) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            x = ""
        )

    return(g)

}
