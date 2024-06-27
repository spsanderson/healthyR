#' Top N tibble
#'
#' @family Data Table Functions
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Get a tibble returned with n records sorted either by descending order (default) or
#' ascending order.
#'
#' @details
#' - Requires a data.frame/tibble
#' - Requires at least one column to be chosen inside of the ...
#' - Will return the tibble in sorted order that is chosen with descending as
#' the default
#'
#' @param .data The data you want to pass to the function
#' @param .n_records How many records you want returned
#' @param .arrange_value A boolean with TRUE as the default. TRUE sorts data in
#' descending order
#' @param ... The columns you want to pass to the function.
#'
#' @examples
#' library(healthyR.data)
#'
#' df <- healthyR_data
#'
#' df_tbl <- top_n_tbl(
#'   .data = df
#'   , .n_records = 3
#'   , .arrange_value = TRUE
#'   , service_line
#'   , payer_grouping
#' )
#'
#' print(df_tbl)
#'
#' @name top_n_tbl
NULL
#' @rdname top_n_tbl
#' @export
#'

top_n_tbl <- function(
    .data
    , .n_records
    , .arrange_value = TRUE
    , ...
) {

    # * Tidyeval ----
    top_n_var_expr <- rlang::enquo(.n_records)
    group_var_expr <- rlang::quos(...)

    arrange_value  <- .arrange_value

    # * Checks ----
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "(data) is not a data-frame/tibble. Please provide.")
    }

    if (rlang::quo_is_missing(top_n_var_expr)) {
        stop(call. = FALSE, "(.n_records) is missing. Please provide.")
    }

    # * Data ----
    data <- tibble::as_tibble(.data)

    # * Manipulate ----
    data_tbl <- data %>%
        dplyr::count(...)

    # Arrange tibble
    if(arrange_value) {
        data_tbl <- data_tbl %>%
            dplyr::arrange(dplyr::desc(n))
    }

    # Slice off n records
    data_tbl <- data_tbl %>%
        dplyr::slice(1:( {{top_n_var_expr}} ))

    # * Return ----
    return(data_tbl)

}

