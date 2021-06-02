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
#' @param .user_input The column that is going to be the row (user)
#' @param .item_input The column that is going to be the column (item)
#'
#' @examples
#' user_item_tbl()
#'
#' @return
#' A aggregated/normalized user item tibble
#'
#' @export
#'

user_item_tbl <- function(.data, .user_input, .item_input){

    # Tidyeval ----
    user_input_var_expr <- rlang::enquo(.user_input)
    item_input_var_expr <- rlang::enquo(.item_input)

    # * Checks ----
    if(!is.data.frame(.data)){
        stop(call. = FALSE, "(.data) is missing, please supply.")
    }

    if(rlang::quo_is_missing(user_input_var_expr)){
        stop(call. = FALSE, "You must supply a user/row input.")
    }

    if(rlang::quo_is_missing(item_input_var_expr)){
        stop(call. = FALSE, "You must supply an item/column input")
    }

}
