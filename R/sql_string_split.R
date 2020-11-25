#' Use SQL LEFT type function
#'
#' @description
#' Perform a sql LEFT() type function on a piece of text
#'
#' @param text A piece of text/string to be manipulated
#' @param num_char How many characters do you want to grab
#'
#' @details
#' - You must supply data that you want to manipulate.
#'
#' @examples
#'
#' sql_left("text", 3)
#'
#' @export
#'

sql_left <- function(text, num_char) {
    base::substr(text, 1, num_char)
}

#' Use SQL MID type function
#'
#' @description
#' Perform a SQL SUBSTRING type function
#'
#' @param text A piece of text/string to be manipulated
#' @param start_num What place to start at
#' @param num_char How many characters do you want to grab
#'
#' @details
#'
#' - You must supply data that you want to manipulate.
#'
#' @examples
#'
#' sql_mid("this is some text", 6, 2)
#'
#' @export
#'

sql_mid <- function(text, start_num, num_char) {
    base::substr(text, start_num, start_num + num_char - 1)
}

#' Use SQL RIGHT type functions
#'
#' @description
#' Perform a SQL RIGHT type function
#'
#' @param text A piece of text/string to be manipulated
#' @param num_char How many characters do you want to grab
#'
#' @details
#'
#' - You must supply data that you want to manipulate.
#'
#' @examples
#'
#' sql_right("this is some more text", 3)
#'
#' @export
#'

sql_right <- function(text, num_char) {
    base::substr(text, base::nchar(text) - (num_char-1), base::nchar(text))
}
