#' Service Line Grouper Augment Function
#'
#' @family Augment Function
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Takes a few arguments from a data.frame/tibble and returns a service line
#' augmented to a data.frame/tibble for a set of patients.
#'
#' @details
#' This is an augment function in that appends a vector to an data.frame/tibble
#' that is passed to the `.data` parameter. A data.frame/tibble
#' is required, along with a principal diagnosis column, a principal procedure column,
#' and a column for the DRG number. These are needed so that the function can
#' join the dx_cc_mapping and px_cc_mapping columns to provide the service line.
#' This function only works on visits that are coded using ICD Version 10 only.
#'
#' Lets take an example discharge, the DRG is 896 and the Principal Diagnosis code
#' maps to DX_660, then this visit would get grouped to `alcohol_abuse`
#'
#' DRG 896: ALCOHOL, DRUG ABUSE OR DEPENDENCE WITHOUT REHABILITATION THERAPY WITH
#' MAJOR COMPLICATION OR COMORBIDITY (MCC)
#'
#' DX_660 Maps to the following ICD-10 Codes ie F1010 Alcohol abuse, uncomplicated:
#'
#' ```
#' library(healthyR)
#' dx_cc_mapping %>%
#'   filter(CC_Code == "DX_660", ICD_Ver_Flag == "10")
#' ```
#'
#' @param .data The data being passed that will be augmented by the function.
#' @param .dx_col The column containing the Principal Diagnosis for the discharge.
#' @param .px_col The column containing the Principal Coded Procedure for the discharge.
#' It is possible that this could be blank.
#' @param .drg_col The DRG Number coded to the inpatient discharge.
#'
#' @examples
#' df <- data.frame(
#'   dx_col = "F10.10",
#'   px_col = NA,
#'   drg_col = "896"
#' )
#'
#' service_line_vec(
#'   .data = df,
#'   .dx_col = dx_col,
#'   .px_col = px_col,
#'   .drg_col = drg_col
#' )
#'
#' @return
#' A vector of service line assignments.
#'
#' @export
#'
