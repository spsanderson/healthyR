#' Gartner Magic Chart Plotting of two continuous variables
#'
#' @description
#' Plot a Gartner Magic Chart of two continuous variabls
#'
#' @param .data The data set you want to plot
#' @param .x_col The x-axis for the plot
#' @param .y_col The y-axis for the plot
#' @param .y_lab The y-axis label
#' @param .x_lab The x-axis label
#' @param .plt_title The title of the plot
#'
#' @details
#' - Supply a data frame with at least two continuous variables to plot against
#' each other
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(purrr)
#' x <- rnorm(180, mean = 0, sd = 1)
#' y <- rnorm(180, mean = 0, sd = 1)
#' df <- data.frame(x,y)
#' df %>%
#'   plt_gartner_magic_chart(
#'     .x_col = x
#'     , .y_col = y
#'     , .x_lab = "Excess LOS"
#'     , .y_lab = "Excess Readmit Rate"
#'     , .plt_title = "Excess LOS vs. Excess Readmit Rate"
#'   )
#'
#' @return
#' A ggplot plot
#'
#' @export
#'
#'

plt_gartner_magic_chart <- function(
    .data
    , .x_col
    , .y_col
    , .y_lab
    , .x_lab
    , .plt_title
) {

    # Tidyeval
    x_var_expr <- rlang::enquo(.x_col)
    y_var_expr <- rlang::enquo(.y_col)
    x_lab_expr <- rlang::enquo(.x_lab)
    y_lab_expr <- rlang::enquo(.y_lab)
    title_expr <- rlang::enquo(.plt_title)

    # Checks
    if(!is.data.frame(.data)) {
        stop(call. = FALSE, "(.data) is not a data.frame/tibble. Please supply.")
    }

    if(rlang::quo_is_missing(x_var_expr)) {
        stop(call. = FALSE, "(.x_col) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(y_var_expr)) {
        stop(call. = FALSE, "(.y_col) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(x_lab_expr)) {
        stop(call. = FALSE, "(.x_lab) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(y_lab_expr)) {
        stop(call. = FALSE, "(.y_lab) is missing. Please supply.")
    }

    if(rlang::quo_is_missing(title_expr)) {
        stop(call. = FALSE, "(.plt_title) is missing. Please supply.")
    }

    data_tbl <- tibble::as_tibble(.data) %>%
        dplyr::select({{x_var_expr}}, {{y_var_expr}}) %>%
        tibble::as_tibble() %>%
        purrr::set_names("x","y")

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
                min(data_tbl$x)
                , max(data_tbl$x)
            )
        ) +
        ggplot2::scale_y_continuous(
            expand = c(0, 0)
            , limits = c(
                min(data_tbl$y)
                , max(data_tbl$y)
            )
        ) +
        # ggplot2::ylab("Excess Readmit Rate") +
        # ggplot2::xlab("Excess LOS") +
        ggplot2::ylab( {{y_lab_expr}} ) +
        ggplot2::xlab( {{x_lab_expr}} ) +
        ggplot2::labs(
            #title = "Gartner Magic Quadrant - Excess LOS vs Excess Readmit Rate"
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
            , xmax = max(x)
            , ymin = 0
            , ymax = max(y)
            , fill = "#F8F9F9"
        ) +
        ggplot2::annotate(
            "rect"
            , xmin = 0
            , xmax = min(x)
            , ymin = 0
            , ymax = min(y)
            , fill = "#F8F9F9"
        ) +
        ggplot2::annotate(
            "rect"
            , xmin = 0
            , xmax = min(x)
            , ymin = 0
            , ymax = max(y)
            , fill = "white"
        ) +
        ggplot2::annotate(
            "rect"
            , xmin = 0
            , xmax = max(x)
            , ymin = 0
            , ymax = min(y)
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
        ggplot2::geom_label(
            ggplot2::aes(
                x = 0.75 * min(x)
                , y = 0.90 * max(y)
                , label = "High RA"
            )
            , label.padding = ggplot2::unit(2, "mm")
            , fill = "lightgrey"
            , color="black"
        ) +
        ggplot2::geom_label(
            ggplot2::aes(
                x = 0.75 * max(x)
                , y = 0.90 * max(y)
                , label = "High RA/LOS"
            )
            , label.padding = ggplot2::unit(2, "mm")
            , fill = "lightgrey"
            , color = "black"
        ) +
        ggplot2::geom_label(
            ggplot2::aes(
                x = 0.75 * min(x)
                , y = 0.90 * min(y)
                , label = "Leader"
            )
            , label.padding = ggplot2::unit(2, "mm")
            , fill = "lightgrey"
            , color = "black"
        ) +
        ggplot2::geom_label(
            ggplot2::aes(
                x = 0.75 * max(x)
                , y = 0.9 * min(y)
                , label = "High LOS"
            )
            , label.padding = ggplot2::unit(2, "mm")
            , fill = "lightgrey"
            , color = "black"
        ) +
        ggplot2::geom_point(
            color = "#2896BA"
            , size = 2
        ) +
        # where you want to be
        ggplot2::geom_point(
            data = data.frame(x = 0, y = 0)
            , ggplot2::aes(color = 'red')
            , size = 3
        )

    return(plt)

}
