#' Time Series Plot
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' This is a warpper function to the `timetke::plot_time_series` function with
#' a limited functionality parameter set. To see the full reference please visit
#' the `timetk` package site.
#'
#' @details This function takes only a few of the arguments in the function and
#' presets others while choosing the defaults on others. The smoother functionality
#' is turned off.
#'
#' @param .data The data to pass to the function, must be a tibble/data.frame
#' @param .date_col The column holding the date
#' @param .value_col The column holding the value
#' @param .color_col The column holding the variable for color
#' @param .facet_col The column holding the variable for faceting
#' @param .facet_ncol How many columns do you want
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

    # Tidyeval
    date_var_expr        <- rlang::enquo(.date_col)
    value_var_expr       <- rlang::enquo(.value_col)
    color_var_expr       <- rlang::enquo(.color_col)
    facet_var_expr       <- rlang::enquo(.facet_col)
    facet_ncol_expr      <- rlang::enquo(.facet_ncol)
    interactive_var_expr <- .interactive

    # Checks
    if(!is.data.frame(.data)) {
        stop(call. = FALSE,"(.data) is not a tibble/data.frame. Please supply")
    }

    if(rlang::quo_is_missing(date_var_expr)){
        stop(call. = FALSE,"(.date_col) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(value_var_expr)){
        stop(call. = FALSE, "(.value_col) is missing. Please supply.")
    }

    data_tbl <- tibble::as_tibble(.data)

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

    return(plt)
}

#' Plot ALOS - Average Length of Stay
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Plot ALOS - Average Length of Stay
#'
#' @details
#' - Expects a tibble with a date time column and a value column
#' - Uses `timetk` for underlying sumarization and plot
#' - If .by_grouping is missing it will default to "day"
#' - A static ggplot2 object is return if the .interactive function is FALSE
#' otherwise a `plotly` plot is returned.
#'
#' @param .data The time series data you need to pass
#' @param .date_col The date column
#' @param .value_col The value column
#' @param .by_grouping How you want the data summarized - "sec", "min", "hour",
#' "day", "week", "month", "quarter" or "year"
#' @param .interactive TRUE or FALSE. TRUE returns a `plotly` plot and FALSE
#' returns a static `ggplot2` plot
#'
#' @examples
#' set.seed(123)
#'
#' suppressPackageStartupMessages(library(timetk))
#' suppressPackageStartupMessages(library(purrr))
#' suppressPackageStartupMessages(library(dplyr))
#'
#' # Make A Series of Dates ----
#' ts_tbl <- tk_make_timeseries(
#'    start = "2019-01-01"
#'    , by = "day"
#'   , length_out = "1 year 6 months"
#')
#'
#' # Set Values ----
#' values <- runif(548, 5, 10)
#'
#' # Make tibble ----
#' df_tbl <- tibble(x = ts_tbl, y = values) %>% set_names("Date","Values")
#'
#' ts_alos_plt(
#' .data = df_tbl, .date_col = Date, .value_col = Values, .by = "month"
#' , .interactive = FALSE
#' )
#'
#' @return
#' A timetk time series plot
#'
#' @export
#'

ts_alos_plt <- function(.data, .date_col, .value_col, .by_grouping, .interactive) {

    # Tidyeval
    date_var_expr        <- rlang::enquo(.date_col)
    value_var_expr       <- rlang::enquo(.value_col)
    by_var_expr          <- .by_grouping
    interactive_var_expr <- .interactive

    # Checks
    if(!is.data.frame(.data)) {
        stop(call. = FALSE, "(data) is not a data-frame or tibble. Please supply.")
    }

    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "(date_var_expr) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(value_var_expr)) {
        stop(call. = FALSE, "(value_var_expr) is missing. Please supply.")
    }

    # Make into a tibble
    df_tbl <- tibble::as_tibble(.data) %>%
        dplyr::select( {{date_var_expr}}, {{value_var_expr}} ) %>%
        purrr::set_names("date", "value")

    # If .by is missing then manipulate
    if(by_var_expr == "") {
        df_summarised_tbl <- timetk::summarise_by_time(
            .data = df_tbl
            , .date_var = date
            , value = mean(value)
        )
    } else {
        df_summarised_tbl <- timetk::summarise_by_time(
            .data = df_tbl
            , .date_var = date
            , .by = by_var_expr
            , value = mean(value)
        )
    }

    # Plot out
    if(!interactive_var_expr) {
        g <- df_summarised_tbl %>%
            timetk::plot_time_series(
                .date_var = date
                , .value = value
                , .title = "Average Length of Stay Plot"
                , .interactive = FALSE
            )
    } else {
        g <- df_summarised_tbl %>%
            timetk::plot_time_series(
                .date_var = date
                , .value = value
                , .title = "Average Length of Stay Plot"
                , .interactive = TRUE
            )
    }

    return(g)

}

#' Plot Readmit Rate
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Plot Readmit Rate
#'
#' @details
#' - Expects a tibble with a date time column and a value column
#' - Uses `timetk` for underlying sumarization and plot
#' - If .by_grouping is missing it will default to "day"
#'
#' @param .data The data you need to pass
#' @param .date_col The date column
#' @param .value_col The value column
#' @param .by_grouping How you want the data summarized - "sec", "min", "hour",
#' "day", "week", "month", "quarter" or "year"
#' @param .interactive TRUE or FALSE. TRUE returns a `plotly` plot and FALSE
#' returns a static `ggplot2` plot
#'
#' @examples
#' set.seed(123)
#'
#' suppressPackageStartupMessages(library(timetk))
#' suppressPackageStartupMessages(library(purrr))
#' suppressPackageStartupMessages(library(dplyr))
#'
#' ts_tbl <- tk_make_timeseries(
#'   start = "2019-01-01"
#'   , by = "day"
#'   , length_out = "1 year 6 months"
#' )
#' values <- arima.sim(
#'   model = list(
#'     order = c(0, 1, 0))
#'     , n = 547
#'     , mean = 1
#'     , sd = 5
#' )
#'
#' df_tbl <- tibble(
#'   x = ts_tbl
#'   , y = values
#'   ) %>%
#'   set_names("Date","Values")
#'
#' ts_readmit_rate_plt(
#'   .data = df_tbl
#'   , .date_col = Date
#'   , .value_col = Values
#'   , .by = "month"
#'   , .interactive = FALSE
#' )
#'
#' @return
#' A `timetk` time series plot that is interactive
#'
#' @export
#'

ts_readmit_rate_plt <- function(.data, .date_col, .value_col, .by_grouping, .interactive) {

    # Tidyeval
    date_var_expr        <- rlang::enquo(.date_col)
    value_var_expr       <- rlang::enquo(.value_col)
    by_var_expr          <- .by_grouping
    interactive_var_expr <- .interactive

    # Checks
    if(!is.data.frame(.data)) {
        stop(call. = FALSE, "(data) is not a data-frame or tibble. Please supply.")
    }

    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "(date_var_expr) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(value_var_expr)) {
        stop(call. = FALSE, "(value_var_expr) is missing. Please supply.")
    }

    # Make into a tibble
    df_tbl <- tibble::as_tibble(.data) %>%
        dplyr::select( {{date_var_expr}}, {{value_var_expr}} ) %>%
        purrr::set_names("date", "value")

    # If .by is missing then manipulate
    if(by_var_expr == "") {
        df_summarised_tbl <- timetk::summarise_by_time(
            .data = df_tbl
            , .date_var = date
            , value = mean(value)
        )
    } else {
        df_summarised_tbl <- timetk::summarise_by_time(
            .data = df_tbl
            , .date_var = date
            , .by = by_var_expr
            , value = mean(value)
        )
    }

    # Plot out
    if(!interactive_var_expr) {
        g <- df_summarised_tbl %>%
            timetk::plot_time_series(
                .date_var = date
                , .value = value
                , .title = "Readmission Rate Plot"
                , .interactive = FALSE
            )
    } else {
        g <- df_summarised_tbl %>%
            timetk::plot_time_series(
                .date_var = date
                , .value = value
                , .title = "Readmission Rate Plot"
                , .interactive = TRUE
            )
    }

    return(g)

}
