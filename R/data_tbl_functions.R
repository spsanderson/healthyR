#' Make LOS and Readmit Index Summary Tibble
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Create the length of stay and readmit index summary tibble
#'
#' @details
#' - Expects a tibble
#' - Expects the following columns and there should only be these 4
#' 1. Length Of Stay Actual - Should be an integer
#' 2. Length Of Stacy Benchmark - Should be an integer
#' 3. Readmit Rate Actual - Should be 0/1 for each record, 1 = readmitted, 0 did not.
#' 4. Readmit Rate Benchmark - Should be a percentage from the benchmark file.
#' - This will add a column called visits that will be the count of records per
#' length of stay from 1 to .max_los
#' - The .max_los param can be left blank and the function will default to 15. If
#' this is not a good default and you don't know what it should be then set it to
#' 75 percentile from the [stats::quantile] function using the defaults, like so
#' .max_los = `stats::quantile(data_tbl$alos)[[4]]`
#' - Uses all data to compute variance, if you want it for a particular time frame
#' you will have to filter the data that goes into the .data argument. It is
#' suggested to use `timetk::filter_by_time()`
#' - The index is computed as the excess of the length of stay or readmit rates
#' over their respective expectations.
#'
#' @param .data The data you are going to analyze.
#' @param .max_los You can give a maximum LOS value. Lets say you typically do
#' not see los over 15 days, you would then set .max_los to 15 and all values greater
#' than .max_los will be grouped to .max_los
#' @param .alos_col The Average Length of Stay column
#' @param .elos_col The Expected Length of Stay column
#' @param .readmit_rate The Actual Readmit Rate column
#' @param .readmit_bench The Expected Readmit Rate column
#'
#' @examples
#'
#' suppressPackageStartupMessages(library(dplyr))
#'
#' data_tbl <- tibble(
#'   "alos"            = runif(186, 1, 20)
#'   , "elos"          = runif(186, 1, 17)
#'   , "readmit_rate"  = runif(186, 0, .25)
#'   , "readmit_bench" = runif(186, 0, .2)
#' )
#'
#' los_ra_index_summary_tbl(
#'   .data = data_tbl
#'   , .max_los       = 15
#'   , .alos_col      = alos
#'   , .elos_col      = elos
#'   , .readmit_rate  = readmit_rate
#'   , .readmit_bench = readmit_bench
#'   )
#'
#' los_ra_index_summary_tbl(
#'   .data = data_tbl
#'   , .max_los       = 10
#'   , .alos_col      = alos
#'   , .elos_col      = elos
#'   , .readmit_rate  = readmit_rate
#'   , .readmit_bench = readmit_bench
#'   )
#'
#' @return
#' A tibble
#'
#' @export
#'

los_ra_index_summary_tbl <- function(
    .data
    , .max_los = 15
    , .alos_col
    , .elos_col
    , .readmit_rate
    , .readmit_bench
    ) {

    # Tidyeval
    max_los_var_expr       <- .max_los
    alos_col_var_expr      <- rlang::enquo(.alos_col)
    elos_col_var_expr      <- rlang::enquo(.elos_col)
    readmit_rate_var_expr  <- rlang::enquo(.readmit_rate)
    readmit_bench_var_expr <- rlang::enquo(.readmit_bench)

    # Checks
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "(data) is not a data-frame/tibble. Please provide.")
    }

    if(!(.max_los)) {
        max_los_var_expr = stats::quantile( !!alos_col_var_expr )[[4]]
    }

    if(rlang::quo_is_missing(alos_col_var_expr)) {
        stop(call. = FALSE, "(.alos_col) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(elos_col_var_expr)) {
        stop(call. = FALSE, "(.elos_col) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(readmit_rate_var_expr)) {
        stop(call. = FALSE, "(.readmit_rate) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(readmit_bench_var_expr)) {
        stop(call. = FALSE, "(.readmit_bench) is missing. Please supply.")
    }

    # Summarize and Manipulate
    df_tbl <- tibble::as_tibble(.data) %>%
        dplyr::mutate(
            alos = {{alos_col_var_expr}} %>%
                as.integer() %>%
                as.double()
            )

    df_summary_tbl <- df_tbl %>%
        dplyr::mutate(
            los_group = dplyr::case_when(
                alos > max_los_var_expr ~ max_los_var_expr
                , TRUE ~ alos
            )
        ) %>%
        dplyr::group_by(los_group) %>%
        dplyr::summarise(
            tot_visits = dplyr::n()
            , tot_los  = sum(alos, na.rm = TRUE)
            , tot_elos = sum({{elos_col_var_expr}}, na.rm = TRUE)
            , tot_ra   = sum({{readmit_rate_var_expr}}, na.rm = TRUE)
            , tot_perf = base::round(base::mean({{readmit_bench_var_expr}}, na.rm = TRUE), digits = 2)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(tot_rar = dplyr::case_when(
            tot_ra != 0 ~ base::round((tot_ra / tot_visits), digits = 2),
            TRUE ~ 0
        )) %>%
        dplyr::mutate(los_index = dplyr::case_when(
            tot_elos != 0 ~ (tot_los / tot_elos),
            TRUE ~ 0
        )) %>%
        dplyr::mutate(rar_index = dplyr::case_when(
            (tot_rar != 0 & tot_perf != 0) ~ (tot_rar / tot_perf),
            TRUE ~ 0
        )) %>%
        dplyr::mutate(
            los_ra_var = base::abs(1 - los_index) + base::abs(1 - rar_index)
        ) %>%
        dplyr::select(los_group, los_index, rar_index, los_ra_var)

    return(df_summary_tbl)

}
