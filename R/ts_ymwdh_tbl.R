#' Make a tibble with year, month, week, week day and hour information added
#'
#' @description
#' Returns a tibble that has year, month, week, week day and hour added from a date column
#'
#' @param .data The data that is being analyzed, data must be a tibble/data.frame
#' @param .date_col The column of the tibble that holds the date
#'
#' @details
#' - Supply data with a date column and this will add the year, month, week, week day and hour
#'   to the tibble. The original date column is kept.
#' - Returns a tibble.
#' - You must know the data going into the function and if certain columns
#'   should be dropped or kept when using further functions
#' - Future work - Add boolean for pad_by_time to fill in missing time series information
#'
#' @examples
#' library(timetk)
#'
#' ts_ymwdh_tbl(
#'   .data = m4_daily
#'   , .date_col = date
#' )
#'
#' @return A tibble
#'
#' @export

ts_ymwdh_tbl <- function(
    .data
    , .date_col
) {

    # Tidayeval Setup
    date_var_expr         <- rlang::enquo(.date_col)

    # Checks
    if(!is.data.frame(.data)) {
        stop(call. = FALSE, "(data) is not a data-frame or tibble. Please supply.")
    }

    # Data Manip
    df_tbl <- tibble::as_tibble(.data) %>%
        dplyr::mutate(yr = lubridate::year({{date_var_expr}})) %>%
        dplyr::mutate(mn = lubridate::month({{date_var_expr}}, label = TRUE)) %>%
        dplyr::mutate(wk = lubridate::isoweek({{date_var_expr}})) %>%
        dplyr::mutate(wd = lubridate::wday( {{date_var_expr}} , label = TRUE)) %>%
        dplyr::mutate(hr = lubridate::hour( {{date_var_expr}} ))

    return(df_tbl)

}
