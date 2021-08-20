#' Diverging Bar Chart
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Diverging Bars is a bar chart that can handle both negative and positive
#' values. This can be implemented by a smart tweak with `geom_bar()`. But the
#' usage of `geom_bar()` can be quite confusing. That's because, it can be used to
#' make a bar chart as well as a histogram. Let me explain.
#'
#' By default, `geom_bar()` has the stat set to count. That means, when you
#' provide just a continuous X variable (and no Y variable), it tries to make
#' a histogram out of the data.
#'
#' In order to make a bar chart create bars instead of histogram,
#' you need to do two things. Set `stat = identity` and provide both `x` and `y`
#' inside `aes()` where, `x` is either character or factor and `y` is numeric.
#' In order to make sure you get diverging bars instead of just bars, make sure,
#' your categorical variable has 2 categories that changes values at a certain
#' threshold of the continuous variable. In below example, the mpg from mtcars
#' data set is normalized by computing the z score. Those vehicles with mpg
#' above zero are marked green and those below are marked red.
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
#' diverging_bar_plt(.data = mtcars, .x_axis = car_name
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
