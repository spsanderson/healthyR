#' Plot Readmit Rate
#'
#' @family Plotting Functions
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
#' @param .data The data you need to pass.
#' @param .date_col The date column.
#' @param .value_col The value column.
#' @param .by_grouping How you want the data summarized - "sec", "min", "hour",
#' "day", "week", "month", "quarter" or "year".
#' @param .interactive TRUE or FALSE. TRUE returns a `plotly` plot and FALSE
#' returns a static `ggplot2` plot.
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

    # * Tidyeval ----
    date_var_expr        <- rlang::enquo(.date_col)
    value_var_expr       <- rlang::enquo(.value_col)
    by_var_expr          <- .by_grouping
    interactive_var_expr <- .interactive

    # * Checks ----
    if(!is.data.frame(.data)) {
        stop(call. = FALSE, "(data) is not a data-frame or tibble. Please supply.")
    }

    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "(date_var_expr) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(value_var_expr)) {
        stop(call. = FALSE, "(value_var_expr) is missing. Please supply.")
    }

    # * Data ----
    df_tbl <- tibble::as_tibble(.data) %>%
        dplyr::select( {{date_var_expr}}, {{value_var_expr}} ) %>%
        purrr::set_names("date", "value")

    # * Manipulate ----
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

    # * Plot ----
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

    # * Return ----
    return(g)

}
