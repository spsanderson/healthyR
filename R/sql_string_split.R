#' Use SQL LEFT, MID and RIGHT type functions
#'
#' @description
#' perform either left, mid or right substring sql like manipulations
#'
#' @param text A piece of text/string to be manipulated
#' @param num_char How many characters do you want to grab
#'
#' @details
#'
#' - You must supply data that you want to manipulate and substrig.
#'
#' @examples
#'
#' left("text", 3)
#'
#' @export

left <- function(text, num_char) {
    base::substr(text, 1, num_char)
}

#' Use SQL LEFT, MID and RIGHT type functions
#'
#' @description
#' perform either left, mid or right substring sql like manipulations
#'
#' @param text A piece of text/string to be manipulated
#' @param start_num What place to start at
#' @param num_char How many characters do you want to grab
#'
#' @details
#'
#' - You must supply data that you want to manipulate and substrig.
#'
#' @examples
#'
#' mid("this is some text", 3, 5)
#'
#' @export
mid <- function(text, start_num, num_char) {
    base::substr(text, start_num, start_num + num_char - 1)
}

#' Use SQL LEFT, MID and RIGHT type functions
#'
#' @description
#' perform either left, mid or right substring sql like manipulations
#'
#' @param text A piece of text/string to be manipulated
#' @param num_char How many characters do you want to grab
#'
#' @details
#'
#' - You must supply data that you want to manipulate and substrig.
#'
#' @examples
#'
#' right("this is some more text", 3)
#'
#' @export
right <- function(text, num_char) {
    base::substr(text, base::nchar(text) - (num_char-1), base::nchar(text))
}
