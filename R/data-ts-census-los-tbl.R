#' Time Series - Census and LOS by Day
#'
#' @family Data Table Functions
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Sometimes it is important to know what the census was on any given day, or what
#' the average length of stay is on given day, including for those patients that
#' are not yet discharged. This can be easily achieved. This will return one
#' record for every account so the data will still need to be summarized. If there
#' are multiple entries per day then those records will show up and you will
#' therefore have multiple entries in the column `date` in the resulting `tibble`.
#' If you want to aggregate from there you should be able to do so easily.
#'
#' If you have a record where the `.start_date_col` is filled in but the corresponding
#' `end_date` is null then the end date will be set equal to `Sys.Date()`
#'
#' If a record has a `start_date` that is `NA` then it will be discarded.
#'
#' __This function can take a little bit of time to run while the join comparison runs.__
#'
#' @details
#' - Requires a dataset that has at least a start date column and an end date
#' column
#' - Takes a single boolean parameter
#'
#' @param .data The data you want to pass to the function
#' @param .keep_nulls_only A boolean that will keep only those records that have
#' a NULL end date, meaning the patient is still admitted. The default is FALSE which
#' brings back all records.
#' @param .start_date_col The column containing the start date for the record
#' @param .end_date_col The column containing the end date for the record.
#' @param .by_time How you want the data presented, defaults to day and should remain
#' that way unless you need more granular data.
#'
#' @examples
#' library(healthyR)
#' library(healthyR.data)
#' library(dplyr)
#'
#' df <- healthyR_data
#'
#' df_tbl <- df %>%
#'   filter(ip_op_flag == "I") %>%
#'   select(visit_start_date_time, visit_end_date_time) %>%
#'   timetk::filter_by_time(.date_var = visit_start_date_time, .start_date = "2020")
#'
#' ts_census_los_daily_tbl(
#'    .data              = df_tbl
#'    , .keep_nulls_only = FALSE
#'    , .start_date_col  = visit_start_date_time
#'    , .end_date_col    = visit_end_date_time
#' )
#'
#' @return
#' A tibble object
#'
#' @export
#'

ts_census_los_daily_tbl <- function(.data, .keep_nulls_only = FALSE,
                                    .start_date_col, .end_date_col,
                                    .by_time = "day"){

    # * Tidyeval Setup ----
    start_date_var_expr <- rlang::enquo(.start_date_col)
    end_date_var_expr   <- rlang::enquo(.end_date_col)
    by_var_expr         <- .by_time
    start_date_var_name <- rlang::quo_name(start_date_var_expr)
    end_date_var_name <- rlang::quo_name(end_date_var_expr)

    # * Checks ----
    if(!is.data.frame(.data)){
        stop(call. = FALSE,"(.data) is not a data.frame/tibble. Please supply.")
    }

    if(rlang::quo_is_missing(start_date_var_expr)){
        stop(call. = FALSE,"(.start_date_col) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(end_date_var_expr)){
        stop(call. = FALSE,"(.end_date_col) is missing. Please supply.")
    }

    keep_nulls_only_bool <- .keep_nulls_only

    # * Data ----
    data_tbl <- tibble::as_tibble(.data)

    # * Manipulate ----
    # Get start date and end date
    all_dates_tbl <- data_tbl %>%
        dplyr::select(
            {{ start_date_var_expr }}
            , {{ end_date_var_expr }}
            , dplyr::everything()
        )

    names(all_dates_tbl)[1] <- "start_date"
    names(all_dates_tbl)[2] <- "end_date"

    all_dates_tbl <- all_dates_tbl %>%
        dplyr::mutate(start_date = as.Date(start_date)) %>%
        dplyr::mutate(end_date   = as.Date(end_date))

    # Filter out records where start_date is.na
    all_dates_tbl <- all_dates_tbl %>%
        dplyr::filter(!is.na(start_date)) %>%

        # If end_date is.na, then make Sys.Date()
        dplyr::mutate(
            end_date = dplyr::case_when(
                is.na(end_date) ~ Sys.Date(),
                TRUE ~ end_date
            )
        )

    # Make calendar dates ----
    start_date <- min(all_dates_tbl[[1]], all_dates_tbl[[2]])
    end_date   <- max(all_dates_tbl[[1]], all_dates_tbl[[2]])
    today      <- Sys.Date()

    ts_day_tbl <- timetk::tk_make_timeseries(
        start_date = start_date
        , end_date = end_date
        , by       = by_var_expr
    ) %>%
        tibble::as_tibble() %>%
        dplyr::rename("date"="value") %>%
        dplyr::mutate(date = as.Date(date))

    # Perform SQL ----
    res <- sqldf::sqldf(
        "
    SELECT B.date,
      A.*
    FROM all_dates_tbl AS A
    LEFT JOIN ts_day_tbl AS B
    ON b.date >= a.start_date
      AND b.date < a.end_date
    ORDER BY b.date
    "
    )

    # Convert to tibble ----
    res_tbl <- tibble::as_tibble(res) %>%
        dplyr::arrange(date)

    los_tbl <- res_tbl %>%
        dplyr::mutate(
            los = dplyr::case_when(
                !is.na(end_date) ~ difftime(
                    end_date, start_date, units = by_var_expr
                ) %>% as.integer()
                , TRUE ~ difftime(
                    today, start_date, units = by_var_expr
                ) %>% as.integer()
            )
        ) %>%
        dplyr::mutate(census = 1) %>%
        dplyr::arrange(date) %>%
        dplyr::rename(!!start_date_var_name := start_date) %>%
        dplyr::rename(!!end_date_var_name := end_date)

    # Keep NA columns?
    if(!keep_nulls_only_bool){
        data_final_tbl <- los_tbl
    } else {
        data_final_tbl <- los_tbl %>%
            dplyr::filter(is.na(end_date))
    }

    # * Return ----
    return(data_final_tbl)

}
