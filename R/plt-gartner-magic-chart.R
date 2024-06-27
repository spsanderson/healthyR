#' Gartner Magic Chart - Plotting of two continuous variables
#'
#' @family Plotting Functions
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Plot a Gartner Magic Chart of two continuous variables.
#'
#' @param .data The dataset you want to plot.
#' @param .x_col The x-axis for the plot.
#' @param .y_col The y-axis for the plot.
#' @param .point_size_col The default is NULL. If you want to size the dots by
#' a column in the data frame/tibble, enter the column name here.
#' @param .y_lab The y-axis label (default: "").
#' @param .x_lab The x-axis label (default: "").
#' @param .plot_title The title of the plot (default: "").
#' @param .top_right_label The top right label (default: "").
#' @param .top_left_label The top left label (default: "").
#' @param .bottom_right_label The bottom right label (default: "").
#' @param .bottom_left_label The bottom left label (default: "").
#'
#' @return
#' A `ggplot` plot.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' data_tbl <- tibble(
#'   x = rnorm(100, 0, 1),
#'   y = rnorm(100, 0, 1),
#'   z = abs(x) + abs(y)
#' )
#'
#' gartner_magic_chart_plt(
#'   .data = data_tbl,
#'   .x_col = x,
#'   .y_col = y,
#'   .point_size_col = z,
#'   .x_lab = "los",
#'   .y_lab = "ra",
#'   .plot_title = "tst",
#'   .top_right_label = "High RA-LOS",
#'   .top_left_label = "High RA",
#'   .bottom_left_label = "Leader",
#'   .bottom_right_label = "High LOS"
#' )
#'
#' gartner_magic_chart_plt(
#'   .data = data_tbl,
#'   .x_col = x,
#'   .y_col = y,
#'   .point_size_col = NULL,
#'   .x_lab = "los",
#'   .y_lab = "ra",
#'   .plot_title = "tst",
#'   .top_right_label = "High RA-LOS",
#'   .top_left_label = "High RA",
#'   .bottom_left_label = "Leader",
#'   .bottom_right_label = "High LOS"
#' )
#' @export

gartner_magic_chart_plt <- function(.data,
                                    .x_col,
                                    .y_col,
                                    .point_size_col = NULL,
                                    .y_lab = "",
                                    .x_lab = "",
                                    .plot_title = "",
                                    .top_left_label = "",
                                    .top_right_label = "",
                                    .bottom_right_label = "",
                                    .bottom_left_label = "") {

    # Tidyeval
    x_var_expr <- rlang::enquo(.x_col)
    y_var_expr <- rlang::enquo(.y_col)
    point_size <- rlang::enquo(.point_size_col)

    # Checks
    if (!is.data.frame(.data)) {
        rlang::abort(
            message = "`data` is not a data.frame/tibble. Please supply.",
            use_cli_format = TRUE
        )
    }

    if (rlang::quo_is_missing(x_var_expr)) {
        rlang::abort(
            message = "`x_col` is missing. Please supply.",
            use_cli_format = TRUE
        )
    }

    if (rlang::quo_is_missing(y_var_expr)) {
        rlang::abort(
            message = "`y_col` is missing. Please supply.",
            use_cli_format = TRUE
        )
    }

    df <- tibble::as_tibble(.data) %>%
        dplyr::select({{ x_var_expr }}, {{ y_var_expr }}) %>%
        purrr::set_names("x", "y")

    xd <- df[["x"]]
    yd <- df[["y"]]

    data_tbl <- dplyr::as_tibble(.data)

    plt <- data_tbl |>
        ggplot2::ggplot(ggplot2::aes(x = {{ x_var_expr }}, y = {{ y_var_expr }})) +
        ggplot2::scale_x_continuous(
            expand = c(0, 0),
            limits = c(
                min(xd) * 1.1,
                max(xd) * 1.1
            )
        ) +
        ggplot2::scale_y_continuous(
            expand = c(0, 0),
            limits = c(
                min(yd) * 1.1,
                max(yd) * 1.1
            )
        ) +
        ggplot2::ylab(.y_lab) +
        ggplot2::xlab(.x_lab) +
        ggplot2::labs(
            title = .plot_title,
            subtitle = "Red Dot Indicates Central Performance"
        ) +
        ggplot2::theme(
            legend.position = "none",
            axis.title.x = ggplot2::element_text(
                hjust = 0,
                vjust = 4,
                colour = "darkgrey",
                size = 10,
                face = "bold"
            ),
            axis.title.y = ggplot2::element_text(
                hjust = 0,
                vjust = 0,
                color = "darkgrey",
                size = 10,
                face = "bold"
            ),
            axis.ticks = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(
                colour = "lightgrey",
                fill = NA,
                size = 4
            )
        ) +
        ggplot2::annotate(
            "rect",
            xmin = 0,
            xmax = max(xd) * 1.1,
            ymin = 0,
            ymax = max(yd) * 1.1,
            fill = "#F8F9F9"
        ) +
        ggplot2::annotate(
            "rect",
            xmin = 0,
            xmax = min(xd) * 1.1,
            ymin = 0,
            ymax = min(yd) * 1.1,
            fill = "#F8F9F9"
        ) +
        ggplot2::annotate(
            "rect",
            xmin = 0,
            xmax = min(xd) * 1.1,
            ymin = 0,
            ymax = max(yd) * 1.1,
            fill = "white"
        ) +
        ggplot2::annotate(
            "rect",
            xmin = 0,
            xmax = max(xd) * 1.1,
            ymin = 0,
            ymax = min(yd) * 1.1,
            fill = "white"
        ) +
        ggplot2::geom_hline(
            yintercept = mean(yd, na.rm = TRUE),
            color = "lightgrey",
            size = 1.5
        ) +
        ggplot2::geom_vline(
            xintercept = mean(xd, na.rm = TRUE),
            color = "lightgrey",
            size = 1.5
        )

    if (rlang::quo_is_null(point_size)) {
        plt <- plt + ggplot2::geom_point(
            color = "#2896BA",
            size = 2
        )
    } else {
        plt <- plt + ggplot2::geom_point(
            color = "#2896BA",
            ggplot2::aes(size = {{ point_size }})
        )
    }

    # Top right label
    if (!is.null(.top_right_label) && .top_right_label != "") {
        plt <- plt +
            ggplot2::geom_label(
                data = data.frame(
                    x = 0.80 * max(xd),
                    y = 0.95 * max(yd)
                ),
                ggplot2::aes(
                    x = x,
                    y = y,
                    label = .top_right_label
                ),
                label.padding = ggplot2::unit(2, "mm"),
                fill = "lightgrey",
                color = "black"
            )
    }

    # Top left label
    if (!is.null(.top_left_label) && .top_left_label != "") {
        plt <- plt +
            ggplot2::geom_label(
                data = data.frame(
                    x = 0.95 * min(xd),
                    y = 0.95 * max(yd)
                ),
                ggplot2::aes(
                    x = x,
                    y = y,
                    label = .top_left_label
                ),
                label.padding = ggplot2::unit(2, "mm"),
                fill = "lightgrey",
                color = "black"
            )
    }

    # Bottom left label
    if (!is.null(.bottom_left_label) && .bottom_left_label != "") {
        plt <- plt +
            ggplot2::geom_label(
                data = data.frame(
                    x = 0.95 * min(xd),
                    y = 0.95 * min(yd)
                ),
                ggplot2::aes(
                    x = x,
                    y = y,
                    label = .bottom_left_label
                ),
                label.padding = ggplot2::unit(2, "mm"),
                fill = "lightgrey",
                color = "black"
            )
    }

    # Bottom right label
    if (!is.null(.bottom_right_label) && .bottom_right_label != "") {
        plt <- plt +
            ggplot2::geom_label(
                data = data.frame(
                    x = 0.80 * max(xd),
                    y = 0.95 * min(yd)
                ),
                ggplot2::aes(
                    x = x,
                    y = y,
                    label = .bottom_right_label
                ),
                label.padding = ggplot2::unit(2, "mm"),
                fill = "lightgrey",
                color = "black"
            )
    }

    # Add center point
    plt <- plt +
        ggplot2::geom_point(
            x = mean(xd, na.rm = TRUE),
            y = mean(yd, na.rm = TRUE),
            color = "red",
            size = 2
        )

    return(plt)
}
