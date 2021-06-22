#' UMAP Projection
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description Create a umap object from the [uwot::umap()] function.
#'
#' @seealso
#' *  \url{https://cran.r-project.org/package=uwot} (CRAN)
#' *  \url{https://github.com/jlmelville/uwot} (GitHub)
#' *  \url{https://github.com/jlmelville/uwot} (arXiv paper)
#'
#' @details This takes in the user item table/matix that is produced by [kmeans_user_item_tbl()]
#' function. This function uses the defaults of [uwot::umap()].
#'
#' @param .data The data from the [kmeans_user_item_tbl()] function.
#' @param .kmeans_map_tbl The data from the [kmeans_mapped_tbl()].
#' @param .k_cluster Pick the desired amount of clusters from your analysis of the scree plot.
#'
#' @examples
#' library(healthyR.data)
#' library(healthyR)
#' library(dplyr)
#' library(broom)
#'
#' data_tbl <- healthyR_data %>%
#'     filter(ip_op_flag == "I") %>%
#'     filter(payer_grouping != "Medicare B") %>%
#'     filter(payer_grouping != "?") %>%
#'     select(service_line, payer_grouping) %>%
#'     mutate(record = 1) %>%
#'     as_tibble()
#'
#' uit_tbl <- kmeans_user_item_tbl(
#'    .data           = data_tbl
#'    , .row_input    = service_line
#'    , .col_input    =  payer_grouping
#'    , .record_input = record
#'  )
#'
#' kmm_tbl <- kmeans_mapped_tbl(uit_tbl)
#'
#' umap_list(.data = uit_tbl, kmm_tbl, 3)
#'
#' @return A list of tibbles and the umap object
#'
#' @export
#'
umap_list <- function(.data
                      , .kmeans_map_tbl
                      , .k_cluster = 5) {
    # * Tidyeval ----
    k_cluster_var_expr <- .k_cluster

    # * Checks ----
    if (!is.data.frame(.data)) {
        stop(call. = FALSE,
             "(.data) is not a data.frame/tibble. Please supply.")
    }

    if (!is.data.frame(.kmeans_map_tbl)) {
        stop(call. = FALSE,
             "(.kmeans_map_tbl) is not a data.frame/tibble. Please supply.")
    }

    # * Data ----
    data           <- tibble::as_tibble(.data)
    kmeans_map_tbl <- tibble::as_tibble(.kmeans_map_tbl)

    # * Manipulation ----
    umap_obj <- data %>%
        dplyr::select(-1) %>%
        uwot::umap()

    umap_results_tbl <- umap_obj %>%
        tibble::as_tibble() %>%
        purrr::set_names("x", "y") %>%
        dplyr::bind_cols(data %>% dplyr::select(1))

    kmeans_obj <- kmeans_map_tbl %>%
        dplyr::pull(k_means) %>%
        purrr::pluck(k_cluster_var_expr)

    kmeans_cluster_tbl <- kmeans_obj %>%
        broom::augment(data) %>%
        dplyr::select(1, .cluster)

    umap_kmeans_cluster_results_tbl <- umap_results_tbl %>%
        dplyr::left_join(kmeans_cluster_tbl)

    # * Data List ----
    list_names <-
        df_list <- list(
            umap_obj                        = umap_obj,
            umap_results_tbl                = umap_results_tbl,
            kmeans_obj                      = kmeans_obj,
            kmeans_cluster_tbl              = kmeans_cluster_tbl,
            umap_kmeans_cluster_results_tbl = umap_kmeans_cluster_results_tbl
        )

    # * Return ----
    return(df_list)

}

#' UMAP and K-Means Cluster Visualization
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description Create a UMAP Projection plot.
#'
#' @seealso
#' *  \url{https://cran.r-project.org/package=uwot} (CRAN)
#' *  \url{https://github.com/jlmelville/uwot} (GitHub)
#' *  \url{https://github.com/jlmelville/uwot} (arXiv paper)
#'
#' @details This takes in `umap_kmeans_cluster_results_tbl` from the [umap_list()]
#' function output.
#'
#' @param .data The data from the [umap_list()] function.
#' @param .point_size The desired size for the points of the plot.
#' @param .label Should [ggrepel::geom_label_repel()] be used to display cluster
#' user labels.
#'
#' @examples
#' library(healthyR.data)
#' library(healthyR)
#' library(dplyr)
#' library(broom)
#' library(ggplot2)
#'
#' data_tbl <- healthyR_data %>%
#'     filter(ip_op_flag == "I") %>%
#'     filter(payer_grouping != "Medicare B") %>%
#'     filter(payer_grouping != "?") %>%
#'     select(service_line, payer_grouping) %>%
#'     mutate(record = 1) %>%
#'     as_tibble()
#'
#' uit_tbl <- kmeans_user_item_tbl(
#'    .data           = data_tbl
#'    , .row_input    = service_line
#'    , .col_input    =  payer_grouping
#'    , .record_input = record
#'  )
#'
#' kmm_tbl <- kmeans_mapped_tbl(uit_tbl)
#'
#' ump_lst <- umap_list(.data = uit_tbl, kmm_tbl, 3)
#'
#' umap_plt(.data = ump_lst, .point_size = 3)
#'
#' @return A ggplot2 UMAP Projection with clusters represented by colors.
#'
#' @export
#'

umap_plt <- function(.data, .point_size = 2, .label = TRUE) {

    # * Checks ----
    if(!is.list(.data)){
        stop(call. = FALSE,"(.data) is not a list")
    }

    # * Data ----
    ump_lst   <- .data
    ump_tbl   <- ump_lst$umap_kmeans_cluster_results_tbl
    optimal_k <- max(ump_lst$kmeans_obj$cluster)

    umap_plt <- ump_tbl %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = x
                , y = y
            )
        ) +
        ggplot2::geom_point(size = .point_size, ggplot2::aes(col = .cluster)) +
        tidyquant::theme_tq() +
        tidyquant::scale_color_tq() +
        ggplot2::labs(
            subtitle = "UMAP 2D Projection with K-Means Cluster Assignment"
            , caption = stringr::str_c(
                "Conclusion:"
                , optimal_k
                , "Clusters Identified"
                , sep = " "
            )
            , color = "Cluster"
        )

    if(.label){
        ump_label <- ump_lst$umap_kmeans_cluster_results_tbl[[3]]

        umap_plt <- umap_plt +
            ggrepel::geom_label_repel(
                mapping = ggplot2::aes(
                    label = ump_label
                )
            )
    }

    # * Return ----
    print(umap_plt)

}
