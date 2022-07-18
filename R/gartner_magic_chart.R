#' Gartner Magic Chart - Plotting of two continuous variables
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Plot a Gartner Magic Chart of two continuous variables
#'
#' @param .data The data set you want to plot
#' @param .x_col The x-axis for the plot
#' @param .y_col The y-axis for the plot
#' @param .point_size_col The default is NULL, if you want to size the dots by
#' a column in the data.frame/tibble then enter the column name here.
#' @param .y_lab The y-axis label
#' @param .x_lab The x-axis label
#' @param .plt_title The title of the plot
#' @param .tr_lbl The top right label
#' @param .tl_lbl The top left label
#' @param .bl_lbl The bottom left label
#' @param .br_lbl The bottom right label
#'
#' @details
#' - Supply a data frame with at least two continuous variables to plot against
#' each other
#'
#' @examples
#' library(dplyr)
#'
#' data_tbl <- tibble(
#'     x = rnorm(100, 0, 1),
#'     y = rnorm(100, 0, 1),
#'     z = abs(x) + abs(y)
#'  )
#'
#' gartner_magic_chart_plt(
#'   .data = data_tbl,
#'   .x_col = x,
#'   .y_col = y,
#'   .point_size = z,
#'   .x_lab = "los",
#'   .y_lab = "ra",
#'   .plt_title = "tst",
#'   .tr_lbl = "High RA-LOS",
#'   .tl_lbl = "High RA",
#'   .bl_lbl = "Leader",
#'   .br_lbl = "High LOS"
#' )
#'
#' gartner_magic_chart_plt(
#'   .data = data_tbl,
#'   .x_col = x,
#'   .y_col = y,
#'   .point_size = NULL,
#'   .x_lab = "los",
#'   .y_lab = "ra",
#'   .plt_title = "tst",
#'   .tr_lbl = "High RA-LOS",
#'   .tl_lbl = "High RA",
#'   .bl_lbl = "Leader",
#'   .br_lbl = "High LOS"
#' )
#'
#' @return
#' A `ggplot` plot
#'
#' @export
#'
#'

gartner_magic_chart_plt <- function(.data,
                                    .x_col,
                                    .y_col,
                                    .point_size_col = NULL,
                                    .y_lab,
                                    .x_lab,
                                    .plt_title,
                                    .tl_lbl,
                                    .tr_lbl,
                                    .br_lbl,
                                    .bl_lbl) {

    # Tidyeval
    x_var_expr <- rlang::enquo(.x_col)
    y_var_expr <- rlang::enquo(.y_col)
    x_lab_expr <- rlang::enquo(.x_lab)
    y_lab_expr <- rlang::enquo(.y_lab)
    title_expr <- rlang::enquo(.plt_title)
    tl_lab_expr <- rlang::enquo(.tl_lbl)
    tr_lab_expr <- rlang::enquo(.tr_lbl)
    bl_lab_expr <- rlang::enquo(.bl_lbl)
    br_lab_expr <- rlang::enquo(.br_lbl)
    point_size <- rlang::enquo(.point_size_col)

    # Checks
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "(.data) is not a data.frame/tibble. Please supply.")
    }

    if (rlang::quo_is_missing(x_var_expr)) {
        stop(call. = FALSE, "(.x_col) is missing. Please supply.")
    }

    if (rlang::quo_is_missing(y_var_expr)) {
        stop(call. = FALSE, "(.y_col) is missing. Please supply.")
    }

    if (rlang::quo_is_missing(x_lab_expr)) {
        stop(call. = FALSE, "(.x_lab) is missing. Please supply.")
    }

    if (rlang::quo_is_missing(y_lab_expr)) {
        stop(call. = FALSE, "(.y_lab) is missing. Please supply.")
    }

    if (rlang::quo_is_missing(title_expr)) {
        stop(call. = FALSE, "(.plt_title) is missing. Please supply.")
    }

    if (rlang::quo_is_missing(point_size)) {
        point_size = 1
    }

    df <- tibble::as_tibble(.data) %>%
        dplyr::select({{ x_var_expr }}, {{ y_var_expr }}) %>%
        tibble::as_tibble() %>%
        purrr::set_names("x", "y")

    xd <- df %>%
        dplyr::pull(x)

    yd <- df %>%
        dplyr::pull(y)

    data_tbl <- dplyr::as_tibble(.data)

    plt <- data_tbl %>%
        ggplot2::ggplot(
            ggplot2::aes(
                x = x
                , y = y
            )
        ) +
        ggplot2::scale_x_continuous(
            expand = c(0, 0)
            , limits = c(
                min(xd * 1.1)
                , max(xd * 1.1)
            )
        ) +
        ggplot2::scale_y_continuous(
            expand = c(0, 0)
            , limits = c(
                min(yd * 1.1)
                , max(yd * 1.1)
            )
        ) +
        ggplot2::ylab( {{y_lab_expr}} ) +
        ggplot2::xlab( {{x_lab_expr}} ) +
        ggplot2::labs(
            title = {{title_expr}}
            , subtitle = "Red Dot Indicates Zero Variance"
        ) +
        ggplot2::theme(
            legend.position = "none"
            , axis.title.x = ggplot2::element_text(
                hjust = 0
                , vjust = 4
                , colour = "darkgrey"
                , size = 10
                , face = "bold"
            )
            , axis.title.y = ggplot2::element_text(
                hjust = 0
                , vjust = 0
                , color = "darkgrey"
                , size = 10
                , face = "bold"
            )
            , axis.ticks = ggplot2::element_blank()
            , panel.border = ggplot2::element_rect(
                colour = "lightgrey"
                , fill = NA
                , size = 4
            )
        ) +
        ggplot2::annotate(
            "rect"
            , xmin = 0
            , xmax = max(xd * 1.1)
            , ymin = 0
            , ymax = max(yd * 1.1)
            , fill = "#F8F9F9"
        ) +
        ggplot2::annotate(
            "rect"
            , xmin = 0
            , xmax = min(xd * 1.1)
            , ymin = 0
            , ymax = min(yd * 1.1)
            , fill = "#F8F9F9"
        ) +
        ggplot2::annotate(
            "rect"
            , xmin = 0
            , xmax = min(xd * 1.1)
            , ymin = 0
            , ymax = max(yd * 1.1)
            , fill = "white"
        ) +
        ggplot2::annotate(
            "rect"
            , xmin = 0
            , xmax = max(xd * 1.1)
            , ymin = 0
            , ymax = min(yd * 1.1)
            , fill = "white"
        ) +
        ggplot2::geom_hline(
            yintercept = 0
            , color = "lightgrey"
            , size = 1.5
        ) +
        ggplot2::geom_vline(
            xintercept = 0
            , color = "lightgrey"
            , size = 1.5
        ) +
        # Where you want to be
        ggplot2::geom_point(
            data = data.frame(x = 0, y = 0)
            , ggplot2::aes(color = 'red')
            , size = 3
        )

    plt <- if(rlang::quo_is_missing(point_size)){
        plt <- plt +
            ggplot2::geom_point(
                color = "#2896BA"
                , size = 2
            )
    } else {
        plt <- plt +
            ggplot2::geom_point(
                color = "#2896BA",
                ggplot2::aes(size = {{point_size}})
            )
    }

    # If statements to add inside labels should they exist
    # Top right label
    if (!rlang::quo_is_missing(tr_lab_expr)) {
        plt <- plt +
            ggplot2::geom_label(
                ggplot2::aes(
                    x = 0.80 * max(xd),
                    y = 0.95 * max(yd),
                    label = {{ tr_lab_expr }}
                ),
                label.padding = ggplot2::unit(2, "mm"),
                fill = "lightgrey",
                color = "black"
            )
    }

    # Top left label
    if (!rlang::quo_is_missing(tl_lab_expr)) {
        plt <- plt +
            ggplot2::geom_label(
                ggplot2::aes(
                    x = 0.95 * min(xd),
                    y = 0.95 * max(yd),
                    label = {{ tl_lab_expr }}
                ),
                label.padding = ggplot2::unit(2, "mm"),
                fill = "lightgrey",
                color = "black"
            )
    }

    # Bottom left label
    if (!rlang::quo_is_missing(bl_lab_expr)) {
        plt <- plt +
            ggplot2::geom_label(
                ggplot2::aes(
                    x = 0.95 * min(xd),
                    y = 0.95 * min(yd),
                    label = {{ bl_lab_expr }}
                ),
                label.padding = ggplot2::unit(2, "mm"),
                fill = "lightgrey",
                color = "black"
            )
    }

    # Bottom right label
    if (!rlang::quo_is_missing(br_lab_expr)) {
        plt <- plt +
            ggplot2::geom_label(
                ggplot2::aes(
                    x = 0.80 * max(xd),
                    y = 0.95 * min(yd),
                    label = {{ br_lab_expr }}
                ),
                label.padding = ggplot2::unit(2, "mm"),
                fill = "lightgrey",
                color = "black"
            )
    }

    return(plt)
}
