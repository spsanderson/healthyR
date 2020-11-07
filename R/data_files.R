#' Diagnosis to Condition Code Mapping file
#'
#' A dataset containing the Diagnosis Code to AHRQ Condition Code Mapping
#' that is used in helping to define service lines for inpatient discharges.
#'
#' \itemize{
#'   \item CC_Code. DX_1, DX_2, ..., DX_n
#'   \item CC_Desc. DX_1 = Conduction disorders, DX_n = description
#'   \item ICD_Ver_Flag. ICD Version 10 or 9
#'   \item ICDCode. ICD-9 ro ICD-10 Code
#'   \item Diagnosis. Long QT Syndrome
#'  }
#'
#' @docType data
#' @keywords datasets
#' @name dx_cc_mapping
#' @usage data(dx_cc_mapping)
#' @format A data frame with 86852 rows and 5 variables
"dx_cc_mapping"

#' Procedure to Condition Code Mapping file
#'
#' A dataset containing the Procedure Code to AHRQ Condition Code Mapping
#' that is used in helping to define servce lines for inpatient discharges.
#'
#' \itemize{
#'   \item CC_Code. PX_1, PX_2, ..., PX_n
#'   \item CC_Desc. PX_1 = Genitourinary incontinence procedures
#'   \item ICD_Ver_Flag. 10 or 9
#'   \item ICDCode. ICD-9 or ICD-10 Code
#'   \item Procedure. Inject Implant Urethra
#' }
#'
#' @docType data
#' @keywords datasets
#' @name px_cc_mapping
#' @usage data(px_cc_mapping)
#' @format A data frame with 79721 rows and 5 variables
"px_cc_mapping"
