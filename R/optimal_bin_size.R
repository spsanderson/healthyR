#' Get the optimal binwidth for a histogram
#'
#' Modified from Hideaki Shimazaki
#' Department of Physics, Kyoto University
#' shimazaki at ton.scphys.kyoto-u.ac.jp
#' Feel free to modify/distribute this program.
#'
#' @description
#' Gives the optimal binwidth for a histogram given a data set, it's value and
#' the desired amount of bins
#'
#' @param .data The data set in question
#' @param .value_col The column that holds the values
#' @param .iters How many times the cost function loop should run
#'
#' @details
#' - Supply a data.frame/tibble with a value column. from this an optimal binwidth
#'   will be computed for the amount of binds desired
#'
#' @examples
#'
#' suppressPackageStartupMessages(library(purrr))
#' suppressPackageStartupMessages(library(dplyr))
#'
#' df_tbl <- rnorm(n = 1000, mean = 0, sd = 1)
#' df_tbl <- df_tbl %>%
#'   as_tibble() %>%
#'   set_names("value")
#'
#' df_tbl %>%
#'   opt_bin(
#'     .value_col = value
#'     , .iters = 100
#'   )
#'
#' @return
#' A tibble of histogram breakpoints
#'
#' @export
#'

opt_bin <- function(
    .data
    , .value_col
    , .iters = 30
    ) {

    # Tidyeval
    value_var_expr  <- rlang::enquo(.value_col)
    iters          <- .iters

    # Checks
    if(!is.data.frame(.data)) {
        stop(call. = FALSE, "(.data) is not a data.frame/tibble. Please supply.")
    }

    if(rlang::quo_is_missing(value_var_expr)) {
        stop(call. = FALSE, "(.value_col) is missing. Please supply.")
    }

    if(is.null(iters)) {
        iters = 30
    }

    # Data Column Pull
    data <- tibble::as_tibble(.data) %>%
        dplyr::select( {{value_var_expr}} ) %>%
        dplyr::pull( {{value_var_expr}} )


    # Get n sequence
    n <- 2:iters
    c <- base::numeric(base::length(n))
    d <- c

    # For Loop
    for (i in 1:length(n)) {

        d[i] <- diff(range( data ) ) / n[i]

        edges = seq(min( data ), max( data ), length = n[i])
        hp <- graphics::hist(data, breaks = edges, plot = FALSE)
        ki <- hp$counts

        k <- mean(ki)
        v <- sum((ki-k)^2/n[i])

        # Cost function
        c[i] <- (2*k - v)/d[i]^2

    }

    idx   <- which.min(c)
    opt_d <- d[idx]

    edges <- seq(min(data), max(data), length = n[idx])
    edges <- tibble::as_tibble(edges)

    return(edges)

}
