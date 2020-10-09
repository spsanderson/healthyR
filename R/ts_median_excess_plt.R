#' Create a plot showing the excess +/- of the median value
#'
#' @description
#' Plot out the excess +/- of the median value grouped by certain time parameters
#'
#' @param .data The data that is being analyzed, data must be a tibble/data.frame
#' @param .date_col The column of the tibble that holds the date
#' @param .value_col The column that holds the value of interest
#' @param .x_axis What is the be the x-axis, day, week, etc.
#' @param .ggplt_group_var What is the resulting ggplot grouped on, e.g. Year
#' @param .secondary_grp_var Typically the same as the x-axis, may not work as expected otherwise
#' @param ... Same as the .ggplot_group_var and .secondary_grp_var
#'
#' @details
#' - Supply data that you want to view and you will see the excess +/- of the median values
#'   over a specified time series tibble
#'
#' @examples
#' library(timetk)
#' library(dplyr)
#'
#' ts_median_excess_plt(
#'   .data = m4_daily
#'   , .date_col = date
#'   , .value_col = value
#'   , .x_axis = wk
#'   , .ggplt_group_var = yr
#'   , .secondary_group_var = wk
#'   , yr
#'   , wk
#' )
#'
#'
#' @export

ts_median_excess_plt <- function(
    .data
    , .date_col
    , .value_col
    , .x_axis
    , .ggplt_group_var
    , .secondary_grp_var
    , ...
) {

    # Tidayeval Setup
    date_var_expr         <- rlang::enquo(.date_col)

    value_var_expr        <- rlang::enquo(.value_col)
    value_var_name        <- rlang::quo_name(value_var_expr)

    x_axis_var_expr       <- rlang::enquo(.x_axis)
    x_axis_var_name       <- rlang::quo_name(x_axis_var_expr)

    ggplot_group_var_expr <- rlang::enquo(.ggplt_group_var)
    ggplot_group_var_name <- rlang::quo_name(ggplot_group_var_expr)

    secondary_group_var_expr <- rlang::enquo(.secondary_grp_var)

    group_vars_expr       <- rlang::quos(...)

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

    if (rlang::quo_is_missing(ggplot_group_var_expr)) {
        stop(call. = FALSE, "(ggplot_group_var_expr) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(secondary_group_var_expr)) {
        stop(call. = FALSE, "(secondary_group_var_expr) is missing. Please supply.")
    }

    if(length(group_vars_expr) <= 1) {
        stop(call. = FALSE, "(group_vars_expr) is missing. Please supply two.")
    }

    # Get .end_date
    .end_date   <- .data %>%
        dplyr::select({{date_var_expr}}) %>%
        dplyr::pull({{date_var_expr}}) %>%
        base::max()

    # Data Manip
    df_grp_tbl <- tibble::as_tibble(.data) %>%
        dplyr::filter(lubridate::year({{date_var_expr}}) >= lubridate::year(.end_date) - 5) %>%
        dplyr::filter(lubridate::year({{date_var_expr}}) <= lubridate::year(.end_date) - 1) %>%
        dplyr::mutate(yr = lubridate::year({{date_var_expr}})) %>%
        dplyr::mutate(mn = lubridate::month({{date_var_expr}}, label = TRUE)) %>%
        dplyr::mutate(wk = lubridate::isoweek({{date_var_expr}})) %>%
        dplyr::mutate(wd = lubridate::wday( {{date_var_expr}} , label = TRUE)) %>%
        dplyr::mutate(hr = lubridate::hour( {{date_var_expr}} )) %>%
        dplyr::select(- ( {{date_var_expr}} )) %>%
        dplyr::group_by(!!! group_vars_expr) %>%
        dplyr::summarise(value = sum( {{value_var_expr}} )) %>%
        dplyr::ungroup() %>%
        dplyr::group_by( {{secondary_group_var_expr}} ) %>%
        dplyr::summarise(median_value = stats::median(value)) %>%
        dplyr::ungroup()

    df_excess_tbl <- tibble::as_tibble(.data) %>%
        dplyr::mutate(yr = lubridate::year( {{date_var_expr}} )) %>%
        dplyr::mutate(mn = lubridate::month( {{date_var_expr}} , label = TRUE)) %>%
        dplyr::mutate(wk = lubridate::isoweek( {{date_var_expr}} )) %>%
        dplyr::mutate(wd = lubridate::wday( {{date_var_expr}} , label = TRUE)) %>%
        dplyr::mutate(hr = lubridate::hour( {{date_var_expr}} )) %>%
        dplyr::select(- ( {{date_var_expr}} )) %>%
        dplyr::group_by(!!! group_vars_expr) %>%
        dplyr::summarise(value = sum( {{value_var_expr}} )) %>%
        dplyr::ungroup() %>%
        dplyr::group_by( {{secondary_group_var_expr}} ) %>%
        dplyr::left_join(df_grp_tbl) %>%
        dplyr::mutate(excess = value - median_value) %>%
        dplyr::ungroup()

    g <- df_excess_tbl %>%
        dplyr::mutate(last_flag = (df_excess_tbl[[1]] == max(df_excess_tbl[[1]]))) %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes_string(
                x = x_axis_var_name
                , group = ggplot_group_var_name
                )
            ) +
        ggplot2::geom_hline(yintercept = 0, col='gray') +
        ggplot2::geom_line(ggplot2::aes(col=last_flag, y = excess)) +
        ggplot2::scale_color_manual(values = c("FALSE"='gray',"TRUE"='red')) +
        ggplot2::guides(col = FALSE) +
        tidyquant::theme_tq()

    return(g)

}
