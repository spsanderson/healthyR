#' OPPE CPOE K-Means
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Takes in a data.frame/tibble and transforms it into an aggregated/normalized
#'  user-item tibble of proportions. The user will need to input the parameters
#'  for the rows/user and the columns/items.
#'
#' @details This function should be used before using a k-mean model. This is
#' commonly referred to as a user item matrix because "users" tend to be on the
#' rows and "items" (e.g. orders) on the columns.
#'
#' @param .data The data that you want to transform
#' @param .row_input The column that is going to be the row (user)
#' @param .col_input The column that is going to be the column (item)
#'
#' @examples
#' user_item_tbl()
#'
#' @return
#' A aggregated/normalized user item tibble
#'
#' @export
#'

user_item_tbl <- function(.data, .row_input, .col_input){

    # Tidyeval ----
    row_input_var_expr <- rlang::enquo(.row_input)
    col_input_var_expr <- rlang::enquo(.col_input)

    # * Checks ----
    if(!is.data.frame(.data)){
        stop(call. = FALSE, "(.data) is not a data.frame/tibble. Please supply.")
    }

    if(rlang::quo_is_missing(row_input_var_expr)){
        stop(call. = FALSE, "You must supply a user/row input.")
    }

    if(rlang::quo_is_missing(col_input_var_expr)){
        stop(call. = FALSE, "You must supply an item/column input")
    }

    # * Data ----
    data_tbl <- tibble::as_tibble(.data)

    data_summarized_tbl <- data_tbl %>%
        dplyr::group_by({{ row_input_var_expr }}, {{ col_input_var_expr }}) %>%
        dplyr::summarise(total_records = sum(record, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        # Normalize proportions
        dplyr::group_by({{ row_input_var_expr }}) %>%
        dplyr::mutate(prop_of_total = total_records / sum(total_records)) %>%
        dplyr::ungroup()

    # User/Item format
    user_item_tbl <- data_summarized_tbl %>%
        dplyr::select({{ row_input_var_expr }}, {{ col_input_var_expr }}, prop_of_total) %>%
        dplyr::mutate(prop_of_total = base::ifelse(
            base::is.na(prop_of_total), 0, prop_of_total
        )) %>%
        tidyr::pivot_wider(
            names_from    = {{ col_input_var_expr }}
            , values_from = prop_of_total
            , values_fill = list(prop_of_total = 0)
        )

    # * Return ----
    return(user_item_tbl)

}
