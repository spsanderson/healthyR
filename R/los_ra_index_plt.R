#' Plot LOS and Readmit Index with Variance
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Plot the index of the length of stay and readmit rate against each other along
#' with the variance
#'
#' @details
#' - Expects a tibble
#' - Expects a Length of Stay and Readmit column, must be numeric
#' - Uses `cowplot` to stack plots
#'
#' @param .data The data supplied from [los_ra_index_summary_tbl()]
#'
#' @examples
#'
#' suppressPackageStartupMessages(library(dplyr))
#'
#' data_tbl <- tibble(
#'   "alos"                 = runif(186, 1, 20)
#'   , "elos"               = runif(186, 1, 17)
#'   , "readmit_rate"       = runif(186, 0, .25)
#'   , "readmit_rate_bench" = runif(186, 0, .2)
#' )
#'
#' los_ra_index_summary_tbl(
#'   .data = data_tbl
#'   , .max_los       = 15
#'   , .alos_col      = alos
#'   , .elos_col      = elos
#'   , .readmit_rate  = readmit_rate
#'   , .readmit_bench = readmit_rate_bench
#' ) %>%
#'   los_ra_index_plt()
#'
#' los_ra_index_summary_tbl(
#'   .data = data_tbl
#'   , .max_los       = 10
#'   , .alos_col      = alos
#'   , .elos_col      = elos
#'   , .readmit_rate  = readmit_rate
#'   , .readmit_bench = readmit_rate_bench
#' ) %>%
#'   los_ra_index_plt()
#'
#' @return
#' A `patchwork` `ggplot2` plot
#'
#' @export
#'

los_ra_index_plt <- function(.data) {

    # * Checks ----
    if(!is.data.frame(.data)) {
        stop(call. = FALSE, "(.data) is not a data.frame/tibble. Please supply.")
    }

    # * Data ----
    df_tbl <- tibble::as_tibble(.data)

    # Set local variables
    min_los_ra_var = tibble::as_tibble(df_tbl) %>%
        dplyr::filter(df_tbl[[4]] == base::min(df_tbl[[4]])) %>%
        dplyr::select(los_group) %>%
        dplyr::pull()

    min_var = tibble::as_tibble(df_tbl) %>%
        dplyr::filter(df_tbl[[1]] == min_los_ra_var) %>%
        dplyr::select(los_ra_var) %>%
        dplyr::pull()

    # * Plot ----
    g <- tibble::as_tibble(df_tbl) %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = los_group,
                y = los_index
            )
        ) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_line(
            mapping = ggplot2::aes(
                y = los_index
            )
        ) +
        ggplot2::geom_point(
            mapping = ggplot2::aes(
                y = rar_index
            )
            , color = "red"
            , size = 3
        ) +
        ggplot2::geom_line(
            mapping = ggplot2::aes(
                y = rar_index
            )
        ) +
        ggplot2::geom_hline(
            yintercept = 1
            , linetype = "dashed"
        ) +
        ggplot2::geom_vline(
            xintercept = min_los_ra_var
            , linetype = "dashed"
        ) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = "LOS Index vs. Readmit Index",
            subtitle = "Black dots are LOS and Red are Readmit",
            y = "LOS/Readmit Index",
            x = "LOS Group"
        )

    g2 <- tibble::as_tibble(df_tbl) %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = los_group,
                y = los_ra_var
            )
        ) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_line() +
        ggplot2::geom_vline(
            xintercept = min_los_ra_var
            , linetype = "dashed"
        ) +
        ggplot2::geom_hline(
            yintercept = min_var,
            linetype = "dashed",
            color = "red"
        ) +
        ggplot2::scale_y_continuous(labels = scales::number) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = "LOS vs Readmit Rate Index Variance",
            subtitle = stringr::str_c(
                "Total LRIV = "
                , base::round(base::sqrt(base::mean(df_tbl$los_ra_var)), digits = 2)
                , "\n"
                , "Minimum Variance at LOS of "
                , min_los_ra_var
                , " Min Var = "
                , base::round(min_var, digits = 4)
                , sep = ""
            ),
            caption = "Encounters with a LOS >= 15 are grouped to LOS Group 15",
            y = "LOS/Readmit Index",
            x = "LOS Group"
        )

    p <- cowplot::plot_grid(g, g2, ncol = 1, nrow = 2)

    # * Return ----
    return(p)

}
