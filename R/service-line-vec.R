#' Service Line Grouper Vectorized Function
#'
#' @family Vectorized Function
#'
#' @author Steven P. Sanderson II, MPH
#'
#' @description
#' Takes a few arguments from a data.frame/tibble and returns a service line
#' vector for a set of patients.
#'
#' @details
#' This is a vectorized function in that it returns a vector. It can be applied
#' inside of a `mutate` statement when using `dplyr` if desired. A data.frame/tibble
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

service_line_vec <- function(.data, .dx_col, .px_col, .drg_col) {

    # Tidyeval ----
    # Grab the columns necessary
    dx_col <- rlang::enquo(.dx_col)
    px_col <- rlang::enquo(.px_col)
    drg_col <- rlang::enquo(.drg_col)

    # Checks ----
    if(!is.data.frame(.data)){
        stop(call. = FALSE, ".data must be supplied.")
    }

    if(rlang::quo_is_missing(dx_col) | rlang::quo_is_missing(px_col) |
       rlang::quo_is_missing(drg_col)){
        stop(call. = FALSE, "The columns .dx_col, .px_col and .drg_col must be supplied.")
    }

    # Copy data
    data <- tibble::as_tibble(.data)

    # Rearrange data and adjust internal table as desired.
    data <- data %>%
        dplyr::select(
            {{dx_col}},
            {{px_col}},
            {{drg_col}},
            dplyr::everything()
        ) %>%
        dplyr::mutate(
            dx_col = gsub({{dx_col}}, pattern = "\\.", replacement = "") %>%
                stringr::str_squish(),
            px_col = gsub({{px_col}}, pattern = "\\.", replacement = "") %>%
                stringr::str_squish(),
            drg_col = stringr::str_squish({{drg_col}}) %>% as.numeric()
        ) %>%
        dplyr::mutate(
            dplyr::across(
                .cols = tidyselect::vars_select_helpers$where(is.character),
                .fns = stringr::str_squish
                )
            )

    # Join dx_cc_mapping and px_cc_mapping
    df <- data %>%
        dplyr::left_join(
            y = healthyR::dx_cc_mapping %>%
                dplyr::filter(ICD_Ver_Flag == '10'),
            by = c("dx_col" = "ICDCode")
        ) %>%
        dplyr::left_join(
            y = healthyR::px_cc_mapping %>%
                dplyr::filter(ICD_Ver_Flag == "10"),
            by = c("px_col" = "ICDCode")
        ) %>%
        dplyr::select(dx_col:drg_col, CC_Code.x, CC_Code.y) %>%
        dplyr::rename(
            dx_cc_code = CC_Code.x,
            px_cc_code = CC_Code.y
        ) %>%
        dplyr::as_tibble()

    # split off inpatient and outpatient and assign service line appropirately
    ip_df <- df %>%
        dplyr::filter(!is.na(drg_col))

    op_df <- df %>%
        dplyr::filter(is.na(drg_col))

    # Main if statement ----
    # Inpatient assignment
    ip_df <- ip_df %>%
        dplyr::mutate(
            service_line = dplyr::case_when(
                drg_col %in% c("896", "897") &
                    dx_cc_code == "DX_660" ~ "alcohol_abuse",
                drg_col %in% c("231", "232", "233", "234", "235", "236") ~ "cabg",
                drg_col %in% c("34", "35", "36", "37", "38", "39") &
                    px_cc_code %in% c("PX_51", "PX_59") ~ "carotid_endarterectomy",
                drg_col %in% c("602", "603") &
                    dx_cc_code == "DX_197" ~ "cellulitis",
                drg_col == "313" &
                    dx_cc_code == "DX_102" ~ "chest_pain",
                drg_col %in% c("291,292,293") &
                    dx_cc_code %in% c("DX_108", "DX_99") ~ "chf",
                drg_col %in% c("190", "191", "192") &
                    dx_cc_code %in% c("DX_127", "DX_128") ~ "copd",
                drg_col %in% c("765", "766") ~ "c_section",
                drg_col %in% c("61", "62", "63", "64", "65", "66") ~ "cva",
                drg_col %in% c("619", "620", "621") &
                    px_cc_code == "PX_74" ~ "bariatric_surgery_inpatient",
                drg_col %in% c("377", "378", "379") ~ "gi_hemorrhage",
                drg_col %in% c("739", "740", "741", "742", "743") &
                    px_cc_code == "PX_124" ~ "hysterectomy",
                drg_col %in% c("469", "470") &
                    px_cc_code %in% c("PX_152", "PX_153") &
                    !dx_cc_code %in% c(
                        "DX_237", "DX_238", "DX_230", "DX_229",
                        "DX_226", "DX_225", "DX_231", "DX_207"
                    ) ~ "joint_replacement",
                drg_col %in% c("417", "418", "419") ~ "laproscopic_cholecystectomy_inpatient",
                drg_col %in% c("582", "583", "584", "585") &
                    px_cc_code == "PX_167" ~ "mastectomy",
                drg_col %in% c("280", "281", "282", "283", "284", "285") ~ "mi",
                drg_col == "795" &
                    dx_cc_code == "DX_218" ~ "newborn",
                drg_col %in% c("193", "194", "195") ~ "pneumonia",
                drg_col %in% c("881", "885") &
                    dx_cc_code == "DX_657" ~ "major_depression_bipolar_disorders",
                drg_col %in% c("885") &
                    dx_cc_code == "DX_659" ~ "schizophrenia",
                drg_col %in% c("246", "247", "248", "249", "250", "251") &
                    px_cc_code == "PX_45" ~ "ptca",
                drg_col %in% c("945", "946") ~ "rehab",
                drg_col == "312" ~ "syncope",
                drg_col %in% c("67", "68", "69") ~ "tia",
                drg_col %in% c("774", "775") ~ "vaginal_delivery",
                drg_col %in% c("216", "217", "218", "219", "220", "221", "266", "267") ~ "valve_procedure",
                (drg_col >= 1 & drg_col <= 8) |
                    (drg_col >= 10 & drg_col <= 14) |
                    (drg_col %in% c(16, 17)) |
                    (drg_col >= 20 & drg_col <= 42) |
                    (drg_col >= 113 & drg_col <= 117) |
                    (drg_col >= 129 & drg_col <= 139) |
                    (drg_col >= 163 & drg_col <= 168) |
                    (drg_col >= 215 & drg_col <= 265) |
                    (drg_col >= 326 & drg_col <= 358) |
                    (drg_col >= 405 & drg_col <= 425) |
                    (drg_col >= 453 & drg_col <= 519) |
                    (drg_col == 520) |
                    (drg_col >= 570 & drg_col <= 585) |
                    (drg_col >= 614 & drg_col <= 630) |
                    (drg_col >= 652 & drg_col <= 675) |
                    (drg_col >= 707 & drg_col <= 718) |
                    (drg_col >= 734 & drg_col <= 750) |
                    (drg_col >= 765 & drg_col <= 780) |
                    (drg_col >= 782 & drg_col <= 804) |
                    (drg_col >= 820 & drg_col <= 830) |
                    (drg_col >= 853 & drg_col <= 858) |
                    (drg_col == 876) |
                    (drg_col >= 901 & drg_col <= 909) |
                    (drg_col >= 927 & drg_col <= 929) |
                    (drg_col >= 939 & drg_col <= 941) |
                    (drg_col >= 955 & drg_col <= 959) |
                    (drg_col >= 969 & drg_col <= 970) |
                    (drg_col >= 981 & drg_col <= 989) ~ "surgical",
                TRUE ~ "medical"
            )
        )

    # Outpatient assignment
    op_df <- op_df %>%
        dplyr::mutate(
            service_line = dplyr::case_when(
                px_col %in% c("4468", "4495", "43770", "43770") ~ "bariatric_surgery_outpatient",
                px_col %in% c(
                    "3721", "3722", "3723", "36013", "93451",
                    "93452", "93456", "93457", "93458", "93549",
                    "93460", "93461", "93462", "93560", "93531",
                    "93532", "93533"
                ) ~ "cardiac_catheterization",
                px_col %in% c(
                    "1311", "1319", "132", "133", "1341",
                    "1342", "1343", "1351", "1359", "1364",
                    "1365", "1366", "66820", "66821", "66830",
                    "66840", "66850", "66852", "66920", "66930",
                    "66940", "66982", "66983", "66984"
                ) ~ "cataract_removal",
                px_col %in% c(
                    "4513", "4514", "4516", "4523", "4524",
                    "4525", "4542", "4543", "43235", "43236",
                    "43237", "43238", "43239", "43240", "43241",
                    "43242", "43257", "43259", "44100", "44360",
                    "44361", "44370", "44377", "44378", "44379",
                    "44385", "44386", "45317", "45320", "45330",
                    "45331", "45332", "45333", "45334", "45335",
                    "45338", "45339", "45341", "45342", "45345",
                    "45378", "45379", "45380", "45381", "45382",
                    "45383", "45384", "45385", "45391", "45392",
                    "G0104", "G0105", "G0121", "S0601"
                ) ~ "colonoscopy_endoscopy",
                px_col %in% c("5123", "5124", "47562", "47563", "47564") ~ "laproscopic_cholecystectomy_outpatient",
                px_col %in% c(
                    "0066", "92920", "92924", "92928", "92933",
                    "92937", "92941", "92943", "C9600", "C9602",
                    "C9604", "C9606", "C9607"
                ) ~ "ptca_outpatient",
                TRUE ~ "general_outpatient"
            )
        )

    # rbind ip and op back together
    df_final_tbl <- rbind(ip_df, op_df)

    # Get the vector of service lines
    svc_line_vec <- df_final_tbl$service_line

    # Return ----
    return(svc_line_vec)
}
