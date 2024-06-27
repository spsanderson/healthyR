#' Save a file to Excel
#'
#' @family Utilities
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Save a tibble/data.frame to an excel `.xlsx` file. The file will automatically
#' with a save_dtime in the format of 20201109_132416 for November 11th, 2020
#' at 1:24:16PM.
#'
#' @details
#' - Requires a tibble/data.frame to be passed to it.
#'
#' @param .data The tibble/data.frame that you want to save as an `.xlsx` file.
#' @param .file_name the name you want to give to the file.
#'
#' @return
#' A saved excel file
#'
#' @name save_to_excel
NULL
#' @rdname save_to_excel
#' @export
#'

save_to_excel <- function(.data, .file_name) {

    # Checks
    if (!is.data.frame(.data) & !is.list(.data)){
        rlang::abort(
            message = ".data is not a data.frame/tibble. Please supply",
            use_cli_format = TRUE
        )
    }

    if (is.list(.data) & !all(unlist(purrr::map(.data, is.data.frame)))) {
        rlang::abort(
            message = ".data is a list, but is not a list of data.frames/tibbles.
            Please supply a valid list."
        )
    }

    if(is.na(.file_name)){
        rlang::abort(
            message = ".file_name must be supplied.",
            use_cli_format = TRUE
        )
    }

    dl <- .data

    # Save Dir
    file_path <- utils::choose.dir()

    # File Name
    file_name <- .file_name
    file_date <- base::Sys.Date() |>
        stringr::str_replace_all("-","")
    file_time <- base::Sys.time() |>
        healthyR::sql_right(5) |>
        stringr::str_replace_all(":","")
    file_name <- base::paste0(
        "\\"
        ,file_name
        ,"_save_dtime_"
        , file_date
        , "_"
        , file_time
        ,".xlsx"
    )

    f_pn <- base::paste0(
        file_path
        , file_name
    )

    # Save file
    writexl::write_xlsx(
        x = dl
        , path = f_pn
    )

}
