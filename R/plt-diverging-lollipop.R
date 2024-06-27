#' Diverging Lollipop Chart
#'
#' @family Plotting Functions
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' This is a diverging lollipop function. Lollipop chart conveys the same
#' information as bar chart and diverging bar. Except that it looks more modern.
#' Instead of geom_bar, I use geom_point and geom_segment to get the lollipops
#' right. Let’s draw a lollipop using the same data I prepared in the previous
#' example of diverging bars.
#'
#' @details
#' This function takes only a few arguments and returns a ggplot2 object.
#'
#' @param .data The data to pass to the function, must be a tibble/data.frame.
#' @param .x_axis The data that is passed to the x-axis. This will also be the
#' `x` and `xend` parameters of the `geom_segment`
#' @param .y_axis The data that is passed to the y-axis. This will also equal the
#' parameters of `yend` and `label`
#' @param .plot_title Default is NULL
#' @param .plot_subtitle Default is NULL
#' @param .plot_caption Default is NULL
#' @param .interactive Default is FALSE. TRUE returns a plotly plot
#'
#' @examples
#' suppressPackageStartupMessages(library(ggplot2))
#'
#' data("mtcars")
#' mtcars$car_name <- rownames(mtcars)
#' mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)
#' mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")
#' mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
#' mtcars$car_name <- factor(mtcars$car_name, levels = mtcars$car_name)
#'
#' diverging_lollipop_plt(.data = mtcars, .x_axis = car_name
#' , .y_axis = mpg_z)
#'
#' @return
#' A `plotly` plot or a `ggplot2` static plot
#'
#' @importFrom plotly ggplotly
#'
#' @export
#'

diverging_lollipop_plt <- function(.data, .x_axis, .y_axis,
                                   .plot_title = NULL, .plot_subtitle = NULL,
                                   .plot_caption = NULL, .interactive = FALSE){

    # * Tidyeval ----
    x_axis_var    <- rlang::enquo(.x_axis)
    y_axis_var    <- rlang::enquo(.y_axis)
    plot_title    <- .plot_title
    plot_subtitle <- .plot_subtitle
    plot_caption  <- .plot_caption
    interact_var  <- .interactive

    # * Checks ----
    if (rlang::quo_is_missing(x_axis_var) | rlang::quo_is_missing(y_axis_var)){
        stop(call. = FALSE, "You must provide both the .x_axis AND .y_axis columns.")
    }

    if(!is.data.frame(.data)){
        stop(call. = FALSE, "(.data) is missing, please supply.")
    }

    if (!is.logical(.interactive)) {
        stop(call. = FALSE, "You must supply either TRUE or FALSE for .interactive")
    }

    # * Data ----
    data_tbl <- tibble::as_tibble(.data)

    # * Plot ----
    g <- ggplot2::ggplot(
        data = data_tbl
        , ggplot2::aes(
            x = {{ x_axis_var }}
            , y = {{ y_axis_var }}
            , label = {{ y_axis_var }}
            )
        ) +
        ggplot2::geom_point(stat = 'identity', fill = "black", size = 6)  +
        ggplot2::geom_segment(
            ggplot2::aes(y = 0,
                         x = {{ x_axis_var }},
                         yend = {{ y_axis_var }},
                         xend = {{ x_axis_var }}),
                     color = "black") +
        ggplot2::geom_text(color = "white", size = 2) +
        ggplot2::labs(
            title    = plot_title,
            subtitle = plot_subtitle,
            caption  = plot_caption
        ) +
        #ggplot2::ylim(-3, 3) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal()

    # * Return ----
    if(interact_var){
        plt <- plotly::ggplotly(g)
    } else {
        plt <- g
    }

    return(plt)

}
