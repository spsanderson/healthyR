#' Make a Time Enhanced Tibble
#'
#' @description
#' Returns a tibble that has:
#' - year
#' - month
#' - week
#' - week
#' - day
#' - hour
#'
#' All added from a chosen date column defined by the `.date_col` parameter.
#'
#' @param .data The data that is being analyzed.
#' @param .date_col The column that holds the date.
#' @param .pad_time Boolean TRUE/FALSE. If TRUE then the [timetk::pad_by_time()]
#' function is called and used on the data.frame before the modification. The
#' default is TRUE.
#'
#' @details
#' - Supply data with a date column and this will add the year, month, week, week day and hour
#'   to the tibble. The original date column is kept.
#' - Returns a tibble.
#' - You must know the data going into the function and if certain columns
#'   should be dropped or kept when using further functions
#'
#' @examples
#' library(timetk)
#'
#' ts_ymwdh_tbl(
#'   .data       = m4_daily
#'   , .date_col = date
#'   , .pad_time = TRUE
#' )
#'
#' @return A tibble
#'
#' @export

ts_ymwdh_tbl <- function(
    .data
    , .date_col
    , .pad_time = TRUE
) {

    # * Tidayeval Setup ----
    date_var_expr <- rlang::enquo(.date_col)
    pad_var_expr  <- .pad_time

    # Checks
    if(!is.data.frame(.data)) {
        stop(call. = FALSE, "(data) is not a data-frame or tibble. Please supply.")
    }

    # * Manipulation ----
    df_tbl <- tibble::as_tibble(.data)

    if(pad_var_expr){
        df_tbl <- df_tbl %>%
            timetk::pad_by_time(
                .date_var = {{ date_var_expr }}
            )
    }

    df_extended_tbl <- df_tbl %>%
        # timetk::step_timeseries_signature({{ date_var_expr }})
        dplyr::mutate(yr = lubridate::year( {{date_var_expr}} )) %>%
        dplyr::mutate(mn = lubridate::month( {{date_var_expr}}, label = TRUE)) %>%
        dplyr::mutate(wk = lubridate::isoweek( {{date_var_expr}} )) %>%
        dplyr::mutate(wd = lubridate::wday( {{date_var_expr}} , label = TRUE)) %>%
        dplyr::mutate(hr = lubridate::hour( {{date_var_expr}} ))

    return(df_extended_tbl)

}
