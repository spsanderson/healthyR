#' Provide Colorblind Compliant Colors
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @details
#' This function is used in others in order to help render plots for those that
#' are color blind.
#'
#' @description
#' 8 Hex RGB color definitions suitable for charts for colorblind people.
#'
#' @examples
#' color_blind()
#'
#' @return
#' A vector of 8 Hex RGB definitions.
#'
#' @export
color_blind <- function(){
    c("#000000", "#E69F00", "#56B4E9",
      "#009E73", "#F0E442", "#0072B2",
      "#D55E00", "#CC79A7")
}

#' Provide Colorblind Compliant Colors
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @details
#' This function is used in others in order to help render plots for those that
#' are color blind.
#'
#' @description
#' 8 Hex RGB color definitions suitable for charts for colorblind people.
#'
#' @param ... Data passed in from a `ggplot` object
#' @param theme Right now this is `hr` only. Anything else will render an error.
#'
#' @return
#' A `gggplot` layer
#'
#' @export
hr_scale_fill_colorblind <- function(..., theme = "hr") {

    pal <- switch(theme,
                  "hr" = unname(color_blind()) %>% rep(100)
    )

    ggplot2::scale_fill_manual(values = pal)
}

#' Provide Colorblind Compliant Colors
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @details
#' This function is used in others in order to help render plots for those that
#' are color blind.
#'
#' @description
#' 8 Hex RGB color definitions suitable for charts for colorblind people.
#'
#' @param ... Data passed in from a `ggplot` object
#' @param theme Right now this is `hr` only. Anything else will render an error.
#'
#' @return
#' A `gggplot` layer
#'
#' @export
hr_scale_color_colorblind = function(..., theme = "hr") {

    pal <- switch(theme,
                  "hr" = unname(color_blind()) %>% rep(100)
    )

    ggplot2::scale_color_manual(values = pal)
}
