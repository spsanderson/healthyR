#' Counts by Category
#'
#' @family Data Table Functions
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Get the counts of a column by a particular grouping if supplied, otherwise just
#' get counts of a column.
#'
#' @details
#' - Requires a data.frame/tibble.
#' - Requires a value column, a column that is going to counted.
#'
#' @param .data The data.frame/tibble supplied.
#' @param .count_col The column that has the values you want to count.
#' @param .arrange_value Defaults to true, this will arrange the resulting tibble
#' in descending order by .count_col
#' @param ... Place the values you want to pass in for grouping here.
#'
#' @examples
#' library(healthyR.data)
#' library(dplyr)
#'
#' healthyR_data %>%
#'   category_counts_tbl(
#'     .count_col = payer_grouping
#'     , .arrange = TRUE
#'     , ip_op_flag
#'   )
#'
#' healthyR_data %>%
#'   category_counts_tbl(
#'     .count_col = ip_op_flag
#'     , .arrange_value = TRUE
#'     , service_line
#'   )
#'
#' @name category_counts_tbl
NULL
#' @rdname category_counts_tbl
#' @export
#'

category_counts_tbl <- function(.data, .count_col,
                                .arrange_value = TRUE,
                                ...){

    # * Tidyeval ----
    count_col_var_expr <- rlang::enquo(.count_col)

    arrange_value      <- .arrange_value

    # * Checks ----
    if(!is.data.frame(.data)){
        stop(call. = FALSE,"(.data) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(count_col_var_expr)){
        stop(call. = FALSE,"(.count_col) is missing. Please supply.")
    }

    # * Data ----
    data <- tibble::as_tibble(.data)

    # * Manipulate ----
    data_tbl <- data %>%
        dplyr::group_by(...) %>%
        dplyr::count({{count_col_var_expr}}) %>%
        dplyr::ungroup()

    if(arrange_value) {
        data_tbl <- data_tbl %>%
            dplyr::arrange(dplyr::desc(n))
    }

    # * Return ----
    return(data_tbl)

}
