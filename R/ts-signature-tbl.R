#' Make a Time Enhanced Tibble
#'
#' @family Data Table Functions
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Returns a tibble that adds the time series signature from the
#' [timetk::tk_augment_timeseries_signature()] function. All added from a chosen
#' date column defined by the `.date_col` parameter.
#'
#' @param .data The data that is being analyzed.
#' @param .date_col The column that holds the date.
#' @param .pad_time Boolean TRUE/FALSE. If TRUE then the [timetk::pad_by_time()]
#' function is called and used on the data.frame before the modification. The
#' default is TRUE.
#' @param ... Grouping variables to be used by [dplyr::group_by()] before using
#' [timetk::pad_by_time()]
#'
#' @details
#' - Supply data with a date column and this will add the year, month, week, week day and hour
#'   to the tibble. The original date column is kept.
#' - Returns a time-series signature tibble.
#' - You must know the data going into the function and if certain columns
#'   should be dropped or kept when using further functions
#'
#' @examples
#' library(timetk)
#'
#' ts_signature_tbl(
#'   .data       = m4_daily
#'   , .date_col = date
#'   , .pad_time = TRUE
#'   , id
#' )
#'
#' @return A tibble
#'
#' @export

ts_signature_tbl <- function(
    .data
    , .date_col
    , .pad_time = TRUE
    , ...
) {

    # * Tidyeval Setup ----
    date_var_expr <- rlang::enquo(.date_col)
    pad_var_expr  <- .pad_time
    grp_var_expr  <- rlang::quos(...)

    # Checks
    if(!is.data.frame(.data)) {
        stop(call. = FALSE, "(data) is not a data-frame or tibble. Please supply.")
    }

    # * Manipulation ----
    df_tbl <- tibble::as_tibble(.data)

    if(pad_var_expr){
        df_tbl <- df_tbl %>%
            dplyr::group_by(!!! grp_var_expr) %>%
            timetk::pad_by_time(
                .date_var = {{ date_var_expr }}
            ) %>%
            dplyr::ungroup()
    }

    df_extended_tbl <- df_tbl %>%
        timetk::tk_augment_timeseries_signature(
            .date_var = {{ date_var_expr }}
        )

    # * Return ----
    return(df_extended_tbl)

}
