#' Diverging Lollipop Chart
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
#' @param .y_axis The data that is passed to the y-axis.
#' @param .label The data that is passed to the `ggplot` `aes` `label` parameter.
#' This will also equal the `yend` parameter of the `geom_segment`.
#' @param .plot_title Default is NULL
#' @param .plot_subtitle Default is NULL
#' @param .plot_caption Default is NULL
#'
#'
#' @examples
#' suppressPackageStartupMessages(library(ggplot2))
#'
#' # Data Prep
#' data("mtcars")
#' mtcars$car_name <- rownames(mtcars)
#' mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)
#' mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")
#' mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
#' mtcars$car_name <- factor(mtcars$car_name, levels = mtcars$car_name)
#'
#' diverging_lollipop_plt(.data = mtcars, .x_axis = car_name
#' , .y_axis = mpg_z, .label = mpg_z)
#'
#' @return
#' A `plotly` plot or a `ggplot2` static plot
#'
#' @export
#'

ts_plt
