#' Tibble to named list
#'
#' @family Data Table Functions
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Takes in a data.frame/tibble and creates a named list from a supplied grouping
#' variable. Can be used in conjunction with [save_to_excel()] to create a new
#' sheet for each group of data.
#'
#' @details
#' - Requires a data.frame/tibble and a grouping column.
#'
#' @param .data The data.frame/tibble.
#' @param .group_col The column that contains the groupings.
#'
#' @examples
#' library(healthyR.data)
#'
#' df <- healthyR_data
#' df_list <- named_item_list(.data = df, .group_col = service_line)
#' df_list
#'
#' @name named_item_list
NULL
#' @export
#' @rdname named_item_list

named_item_list <- function(.data, .group_col) {
    # * Tidyeval ----
    group_var_expr <- rlang::enquo(.group_col)

    # * Checks ----
    if (!is.data.frame(.data)) {
        stop(call. = FALSE,
             "(.data) is not a data.frame/tibble. Please supply")
    }

    if (rlang::quo_is_missing(group_var_expr)) {
        stop(call. = FALSE, "(.group_col) is missing. Please supply")
    }

    # * Manipulate ----
    data_tbl <- tibble::as_tibble(.data)

    data_tbl_list <- data_tbl %>%
        dplyr::group_split({
            {
                group_var_expr
            }
        })

    names(data_tbl_list) <- data_tbl_list %>%
        purrr::map( ~ dplyr::pull(., {
            {
                group_var_expr
            }
        })) %>%
        purrr::map( ~ base::as.character(.)) %>%
        purrr::map( ~ base::unique(.))

    # * Return ----
    return(data_tbl_list)

}
