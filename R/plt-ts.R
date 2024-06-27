#' Time Series Plot
#'
#' @family Plotting Functions
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' This is a warpper function to the [timetk::plot_time_series()] function with
#' a limited functionality parameter set. To see the full reference please visit
#' the `timetk` package site.
#'
#' @details This function takes only a few of the arguments in the function and
#' presets others while choosing the defaults on others. The smoother functionality
#' is turned off.
#'
#' @param .data The data to pass to the function, must be a tibble/data.frame.
#' @param .date_col The column holding the date.
#' @param .value_col The column holding the value.
#' @param .color_col The column holding the variable for color.
#' @param .facet_col The column holding the variable for faceting.
#' @param .facet_ncol How many columns do you want.
#' @param .interactive Return a `plotly` plot if set to TRUE and a static `ggplot2`
#' plot if set to FALSE. The default is FALSE.
#'
#' @seealso
#' \url{https://business-science.github.io/timetk/reference/plot_time_series.html}
#'
#' @examples
#' suppressPackageStartupMessages(library(dplyr))
#' library(timetk)
#' library(healthyR.data)
#'
#' healthyR.data::healthyR_data %>%
#'   filter(ip_op_flag == "I") %>%
#'   select(visit_end_date_time, service_line) %>%
#'   filter_by_time(
#'     .date_var = visit_end_date_time
#'     , .start_date = "2020"
#'     ) %>%
#'   group_by(service_line) %>%
#'   summarize_by_time(
#'     .date_var = visit_end_date_time
#'     , .by = "month"
#'     , visits = n()
#'   ) %>%
#'  ungroup() %>%
#'  ts_plt(
#'    .date_col = visit_end_date_time
#'    , .value_col = visits
#'    , .color_col = service_line
#'  )
#'
#' @return
#' A `plotly` plot or a `ggplot2` static plot
#'
#' @export
#'

ts_plt <- function(
        .data
        , .date_col
        , .value_col
        , .color_col   = NULL
        , .facet_col   = NULL
        , .facet_ncol  = NULL
        , .interactive = FALSE
) {

    # I* Tidyeval ----
    date_var_expr        <- rlang::enquo(.date_col)
    value_var_expr       <- rlang::enquo(.value_col)
    color_var_expr       <- rlang::enquo(.color_col)
    facet_var_expr       <- rlang::enquo(.facet_col)
    facet_ncol_expr      <- rlang::enquo(.facet_ncol)
    interactive_var_expr <- .interactive

    # * Checks ----
    if(!is.data.frame(.data)) {
        stop(call. = FALSE,"(.data) is not a tibble/data.frame. Please supply")
    }

    if(rlang::quo_is_missing(date_var_expr)){
        stop(call. = FALSE,"(.date_col) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(value_var_expr)){
        stop(call. = FALSE, "(.value_col) is missing. Please supply.")
    }

    # * Data ----
    data_tbl <- tibble::as_tibble(.data)

    # * Plot ----
    plt <- data_tbl %>%
        timetk::plot_time_series(
            .date_var      = {{ date_var_expr }}
            , .value       = {{ value_var_expr }}
            , .color_var   = {{ color_var_expr }}
            , .facet_vars  = {{ facet_var_expr }}
            , .facet_ncol  = {{ facet_ncol_expr }}
            , .interactive = interactive_var_expr
            , .smooth      = FALSE
        )

    # * Return ----
    return(plt)
}
