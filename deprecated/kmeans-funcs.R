#' K-Means Functions
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
#' rows and "items" (e.g. orders) on the columns. You must supply a column that
#' can be summed for the aggregation and normalization process to occur.
#'
#' @param .data The data that you want to transform
#' @param .row_input The column that is going to be the row (user)
#' @param .col_input The column that is going to be the column (item)
#' @param .record_input The column that is going to be summed up for the aggregattion
#' and normalization process.
#'
#' @examples
#' library(healthyR.data)
#' library(dplyr)
#'
#' data_tbl <- healthyR_data%>%
#'    filter(ip_op_flag == "I") %>%
#'    filter(payer_grouping != "Medicare B") %>%
#'    filter(payer_grouping != "?") %>%
#'    select(service_line, payer_grouping) %>%
#'    mutate(record = 1) %>%
#'    as_tibble()
#'
#'  kmeans_user_item_tbl(
#'    .data           = data_tbl
#'    , .row_input    = service_line
#'    , .col_input    =  payer_grouping
#'    , .record_input = record
#'  )
#'
#' @return
#' A aggregated/normalized user item tibble
#'
#' @export
#'

kmeans_user_item_tbl <- function(.data, .row_input, .col_input, .record_input){

    # * Tidyeval ----
    row_input_var_expr <- rlang::enquo(.row_input)
    col_input_var_expr <- rlang::enquo(.col_input)
    rec_input_var_expr <- rlang::enquo(.record_input)

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

    if(rlang::quo_is_missing(rec_input_var_expr)){
        stop(call. = FALSE, "You must supply a record column that can be summed up
             for the aggregation and normalization process.")
    }

    # * Data ----
    data_tbl <- tibble::as_tibble(.data)

    # * Manipulate ----
    data_summarized_tbl <- data_tbl %>%
        dplyr::group_by({{ row_input_var_expr }}, {{ col_input_var_expr }}) %>%
        dplyr::summarise(total_records = sum({{ rec_input_var_expr }}, na.rm = TRUE)) %>%
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


#' K-Means Functions
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Takes the output of the [kmeans_user_item_tbl()] function and applies the
#' k-means algorithm to it using [stats::kmeans()]
#'
#' @details Uses the [stats::kmeans()] function and creates a wrapper around it.
#'
#' @param .data The data that gets passed from [kmeans_user_item_tbl()]
#' @param .centers How many initial centers to start with
#'
#' @examples
#' library(healthyR.data)
#' library(dplyr)
#'
#' data_tbl <- healthyR_data%>%
#'    filter(ip_op_flag == "I") %>%
#'    filter(payer_grouping != "Medicare B") %>%
#'    filter(payer_grouping != "?") %>%
#'    select(service_line, payer_grouping) %>%
#'    mutate(record = 1) %>%
#'    as_tibble()
#'
#'  kmeans_user_item_tbl(
#'    .data           = data_tbl
#'    , .row_input    = service_line
#'    , .col_input    =  payer_grouping
#'    , .record_input = record
#'  ) %>%
#'    kmeans_obj()
#'
#' @return
#' A stats k-means object
#'
#' @export
#'

kmeans_obj <- function(.data, .centers = 5){

    # * Tidyeval ----
    centers_var_expr <- .centers

    # * Checks ----
    if(!is.data.frame(.data)){
        stop(call. = FALSE("(.data) is missing. Please supply."))
    }

    # Default to 5
    if(is.null(centers_var_expr)){centers_var_expr = 5}

    # * Data ----
    data <- tibble::as_tibble(.data)

    # * k-means ----
    kmeans_tbl <- data %>%
        dplyr::select(-1)

    kmeans_obj <- kmeans_tbl %>%
        stats::kmeans(
            centers = centers_var_expr
            , nstart = 100
        )

    return(kmeans_obj)

}

#' K-Means tidy Functions
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' K-Means tidy functions
#'
#' @details
#' Takes in a k-means object and its associated user item tibble and then
#' returns one of the items asked for. Either: [broom::tidy()], [broom::glance()]
#' or [broom::augment()]. The function defaults to [broom::tidy()].
#'
#' @param .kmeans_obj A [stats::kmeans()] object
#' @param .tidy_type "tidy","glance", or "augment"
#' @param .data The user item tibble created from [kmeans_user_item_tbl()]
#'
#' @examples
#' library(healthyR.data)
#' library(dplyr)
#' library(broom)
#'
#' data_tbl <- healthyR_data%>%
#'    filter(ip_op_flag == "I") %>%
#'    filter(payer_grouping != "Medicare B") %>%
#'    filter(payer_grouping != "?") %>%
#'    select(service_line, payer_grouping) %>%
#'    mutate(record = 1) %>%
#'    as_tibble()
#'
#'  uit_tbl <- kmeans_user_item_tbl(
#'    .data           = data_tbl
#'    , .row_input    = service_line
#'    , .col_input    =  payer_grouping
#'    , .record_input = record
#'  )
#'
#'  km_obj  <- kmeans_obj(uit_tbl)
#'
#'  kmeans_tidy_tbl(
#'    .kmeans_obj  = km_obj
#'    , .data      = uit_tbl
#'    , .tidy_type = "augment"
#'  )
#'
#'  kmeans_tidy_tbl(
#'    .kmeans_obj  = km_obj
#'    , .data      = uit_tbl
#'    , .tidy_type = "glance"
#'  )
#'
#'  kmeans_tidy_tbl(
#'    .kmeans_obj  = km_obj
#'    , .data      = uit_tbl
#'    , .tidy_type = "tidy"
#'  ) %>%
#'    glimpse()
#'
#' @return
#' A tibble
#'
#' @export
#'

kmeans_tidy_tbl <- function(.kmeans_obj, .data, .tidy_type = "tidy") {

    # * Tidyeval ----
    kmeans_obj   <- .kmeans_obj
    tidy_type    <- .tidy_type

    # * Checks ----
    if(!is.data.frame(.data)){
        stop(call. = FALSE, "(.user_item_data) is not a data.frame/tibble, please supply original user item tibble.")
    }

    if (!inherits(x = kmeans_obj, what = "kmeans")){
        stop(call. = FALSE, "(.kmeans_obj) is not of class 'kmeans'")
    }
    # if (!class(kmeans_obj) == "kmeans") {
    #     stop(call. = FALSE, "(.kmeans_obj) is not of class 'kmeans'")
    # }

    if (!tidy_type %in% c("tidy", "augment", "glance")) {
        stop(call. = FALSE,
             "(.tidy_type) must be either tidy, glance, or augment")
    }

    # * Manipulate ----
    uit_tbl <- tibble::as_tibble(.data)
    row_col <- colnames(uit_tbl[1])

    if (tidy_type == "tidy") {
        km_tbl <- kmeans_obj %>% broom::tidy()
    } else if (tidy_type == "glance") {
        km_tbl <- kmeans_obj %>% broom::glance()
    } else if (tidy_type == "augment") {
        km_tbl <- kmeans_obj %>%
            broom::augment(uit_tbl) %>%
            dplyr::select(row_col, .cluster) %>%
            dplyr::rename("cluster" = .cluster)
    }

    # * Return ----
    return(km_tbl)

}

#' K-Means Mapper
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description Create a tibble that maps the [kmeans_obj()] using [purrr::map()]
#' to create a nested data.frame/tibble that holds n centers. This tibble will be
#' used to help create a scree plot.
#'
#' @seealso
#' \url{https://en.wikipedia.org/wiki/Scree_plot}
#'
#' @details Takes in a single parameter of .centers. This is used to create the tibble
#' and map the [kmeans_obj()] function down the list creating a nested tibble.
#'
#' @param .centers How many different centers do you want to try
#' @param .data You must have a tibble in the working environment from the
#' [kmeans_user_item_tbl()]
#'
#' @examples
#' library(healthyR.data)
#' library(dplyr)
#'
#' data_tbl <- healthyR_data%>%
#'    filter(ip_op_flag == "I") %>%
#'    filter(payer_grouping != "Medicare B") %>%
#'    filter(payer_grouping != "?") %>%
#'    select(service_line, payer_grouping) %>%
#'    mutate(record = 1) %>%
#'    as_tibble()
#'
#' ui_tbl <-  kmeans_user_item_tbl(
#'    .data           = data_tbl
#'    , .row_input    = service_line
#'    , .col_input    =  payer_grouping
#'    , .record_input = record
#'  )
#'
#' kmeans_mapped_tbl(ui_tbl)
#'
#' @return
#' A nested tibble
#'
#' @export
#'
kmeans_mapped_tbl <- function(.data, .centers = 15){

    # * Tidyeval ----
    centers_var_expr <- .centers

    # * Checks ----
    if(!is.data.frame(.data)){
        stop(call. = FALSE, "(.data) is not a data.frame/tibble. Please supply.")
    }

    input_data <- tibble::as_tibble(.data)

    km_mapper <- function(centers = 3){
        input_data %>%
            dplyr::select(-1) %>%
            stats::kmeans(
                centers = centers
                , nstart = 100
            )
    }

    # * Manipulate ----
    data_tbl <- tibble::tibble(centers = 1:centers_var_expr) %>%
        dplyr::mutate(k_means = centers %>%
                          purrr::map(km_mapper)
        ) %>%
        dplyr::mutate(glance = k_means %>%
                          purrr::map(broom::glance))

    # * Return ----
    return(data_tbl)

}

#' K-Means Scree Plot Data Table
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description Take data from the [kmeans_mapped_tbl()] and unnest it into a
#' tibble for inspection and for use in the [kmeans_scree_plt()] function.
#'
#' @details Takes in a single parameter of .data from [kmeans_mapped_tbl()] and
#' transforms it into a tibble that is used for [kmeans_scree_plt()]. It will
#' show the values (tot.withinss) at each center.
#'
#' @param .data You must have a tibble in the working environment from the
#' [kmeans_mapped_tbl()]
#'
#' @examples
#' library(healthyR.data)
#' library(dplyr)
#'
#' data_tbl <- healthyR_data%>%
#'    filter(ip_op_flag == "I") %>%
#'    filter(payer_grouping != "Medicare B") %>%
#'    filter(payer_grouping != "?") %>%
#'    select(service_line, payer_grouping) %>%
#'    mutate(record = 1) %>%
#'    as_tibble()
#'
#' ui_tbl <-  kmeans_user_item_tbl(
#'    .data           = data_tbl
#'    , .row_input    = service_line
#'    , .col_input    =  payer_grouping
#'    , .record_input = record
#'  )
#'
#' kmm_tbl <- kmeans_mapped_tbl(ui_tbl)
#'
#' kmeans_scree_data_tbl(kmm_tbl)
#'
#' @return
#' A nested tibble
#'
#' @export
#'
kmeans_scree_data_tbl <- function(.data) {

    # * Checks ----
    if(!is.data.frame(.data)){
        stop(call. = FALSE, "(.data) is not a data.frame/tibble. Please supply.")
    }

    # * Manipulate ----
    data_tbl <- tibble::as_tibble(.data)

    data_tbl <- data_tbl %>%
        tidyr::unnest(glance) %>%
        dplyr::select(centers, tot.withinss)

    # * Return ----
    return(data_tbl)

}

#' K-Means Scree Plot
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description Create a scree-plot from the [kmeans_mapped_tbl()] function.
#'
#' @details Outputs a scree-plot
#'
#' @seealso
#' \url{https://en.wikipedia.org/wiki/Scree_plot}
#'
#' @param .data The data from the [kmeans_mapped_tbl()] function
#'
#' @examples
#' library(healthyR.data)
#' library(dplyr)
#'
#' data_tbl <- healthyR_data%>%
#'    filter(ip_op_flag == "I") %>%
#'    filter(payer_grouping != "Medicare B") %>%
#'    filter(payer_grouping != "?") %>%
#'    select(service_line, payer_grouping) %>%
#'    mutate(record = 1) %>%
#'    as_tibble()
#'
#' ui_tbl <-  kmeans_user_item_tbl(
#'    .data           = data_tbl
#'    , .row_input    = service_line
#'    , .col_input    =  payer_grouping
#'    , .record_input = record
#'  )
#'
#' kmm_tbl <- kmeans_mapped_tbl(ui_tbl)
#'
#' kmeans_scree_plt(.data = kmm_tbl)
#'
#' @return
#' A ggplot2 plot
#'
#' @export
#'

kmeans_scree_plt <- function(.data){

    # * Checks ----
    if(!is.data.frame(.data)){
        stop(call. = FALSE,"(.data) is not a data.frame/tibble. Please supply.")
    }

    # * Manipulate ----
    data_tbl <- tibble::as_tibble(.data)

    data_tbl <- data_tbl %>%
        tidyr::unnest(glance) %>%
        dplyr::select(centers, tot.withinss)

    # * Plot
    p <- data_tbl %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x   = centers
                , y = tot.withinss
            )
        ) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        # Kaiser Line
        ggplot2::geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
        ggrepel::geom_label_repel(
            mapping = ggplot2::aes(
                label = centers
            )) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title      = "Scree Plot"
            , subtitle = "Measures the distance each of the users are from the closest k-means cluster"
            , y        = "Total Within Sum of Squares"
            , x        = "Centers"
        )

    # * Return ----
    return(p)
}
