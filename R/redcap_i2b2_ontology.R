#' Build an i2b2 ontology table from a REDCap dictionary
#'
#' @param redcap_tidy_dd REDCap tidy data dictionary
#' @param project_id Project name to use as the root level (default: REDCap)
#' @param concept_cd_prefix Prefix to prepend to C_BASECODE (default: RC:)
#' @param include_forms Forms to include in the i2b2 ontology.  The default is
#' to include all forms
#'
#' @return data.frame
#' @export
#' @importFrom rlang .data
redcap_i2b2_ontology <- function(redcap_tidy_dd,
                                 project_id = "REDCap",
                                 concept_cd_prefix = "RC:",
                                 include_forms = NA) {


  dd <- redcap_tidy_dd |>
    dplyr::filter(
      .data$field_type %in% c(
        "checkbox",
        "radio",
        "dropdown",
        "text",
        "yesno",
        "truefalse",
        "notes",
        "calc"
      )
    )

  if (!is.na(include_forms)) {
    dd <- dd |>
      dplyr::filter(.data$form_name %in% include_forms)
  }

  concept_staging <- NULL


  #LEVEL 0
  concept_staging <- data.frame(
    C_BASECODE = NA,
    C_NAME = project_id,
    C_FULLNAME = stringr::str_glue("\\{project_id}\\"),
    C_VISUALATTRIBUTES = "CA",
    C_COMMENT = NA,
    C_METADATAXML = NA,
    C_HLEVEL = 1,
    C_TOOLTIP = project_id
  )


  #LEVEL 1: FORMS
  concept_staging <- dplyr::bind_rows(
    concept_staging,
    dd |>
      dplyr::transmute(
        C_BASECODE = stringr::str_glue("{concept_cd_prefix}{form_name}"),
        C_NAME = stringr::str_glue("{instrument_label}"),
        C_FULLNAME = stringr::str_glue("\\{project_id}\\{form_name}\\"),
        C_VISUALATTRIBUTES = "FAE",
        C_HLEVEL = stringr::str_count(.data$C_FULLNAME, "\\\\") - 1,
        C_TOOLTIP = stringr::str_glue("{project_id} \\ {instrument_label} \\"),
        i_form_name = .data$form_name
      ) |>
      unique()
  )




  #LEVEL 2: SECTIONS
  if (dd |>
      dplyr::filter(!is.na(.data$section_header) &
                    .data$section_header != "") |>
      nrow() > 0) {
    concept_staging <- dplyr::bind_rows(
      concept_staging,
      dd |>
        dplyr::filter(!is.na(.data$section_header) &
                        .data$section_header != "") |>
        dplyr::transmute(
          C_BASECODE = stringr::str_glue("{concept_cd_prefix}{section_cd}"),
          C_NAME = stringr::str_glue(
            "(S-{stringr::str_pad(section_order, 2, 'left', pad='0')})
            {section_header}"
          ),
          C_FULLNAME =
            stringr::str_glue("\\{project_id}\\{form_name}\\{section_cd}\\"),
          C_VISUALATTRIBUTES = "FAE",
          C_HLEVEL = stringr::str_count(.data$C_FULLNAME, "\\\\") - 1,
          C_TOOLTIP =
            stringr::str_glue("{project_id} \\ {instrument_label} \\
                              {section_header}"),
          i_form_name = .data$form_name,
          i_section_name = .data$section_header
        ) |>
        unique()
    )
  }


  #LEVEL 3: FIELDS
  concept_staging <- dplyr::bind_rows(
    concept_staging,
    dd |>
      dplyr::filter(is.na(.data$choice_value)) |>
      tidyr::unite(
        "path",
        "form_name",
        "section_cd",
        "field_name",
        sep = "\\",
        na.rm = TRUE,
        remove = FALSE
      ) |>
      tidyr::unite(
        "tooltip",
        "instrument_label",
        "section_header",
        "field_label",
        sep = " \\ ",
        na.rm = TRUE,
        remove = FALSE
      ) |>
      dplyr::transmute(
        C_BASECODE = stringr::str_glue("{concept_cd_prefix}{field_name}"),
        C_NAME = stringr::str_glue(
          "({stringr::str_pad(field_order, 3, 'left', pad='0')}) {field_label}"
        ),
        C_FULLNAME = paste0("\\", project_id, "\\", .data$path, "\\"),
        C_VISUALATTRIBUTES = ifelse(.data$field_type %in% c("text", "notes"),
                                    "LAE", "FAE"),
        C_HLEVEL = stringr::str_count(.data$C_FULLNAME, "\\\\") - 1,
        C_TOOLTIP = stringr::str_glue("{project_id} \\ {tooltip}"),
        C_METADATAXML = ifelse(
          .data$text_validation_type_or_show_slider_number == "integer" |
            grepl("number", .data$text_validation_type_or_show_slider_number),
          stringr::str_glue(
            '<?xml version="1.0"?><ValueMetadata><Version>3.02</Version>
            <CreationDateTime>{Sys.time()}</CreationDateTime><TestID></TestID>
            <TestName>{concept_name}</TestName><DataType>Float</DataType>
            <CodeType></CodeType><Loinc></Loinc><Flagstouse />
            <Oktousevalues>N</Oktousevalues><MaxStringLength></MaxStringLength>
            <LowofLowValue></LowofLowValue><HighofLowValue></HighofLowValue>
            <LowofHighValue></LowofHighValue><HighofHighValue></HighofHighValue>
            <LowofToxicValue></LowofToxicValue><HighofToxicValue>
            </HighofToxicValue><EnumValues></EnumValues>
            <CommentsDeterminingExclusion><Com></Com>
            </CommentsDeterminingExclusion><UnitValues><NormalUnits>
            </NormalUnits><EqualUnits></EqualUnits><ExcludingUnits>
            </ExcludingUnits><ConvertingUnits><Units></Units><MultiplyingFactor>
            </MultiplyingFactor></ConvertingUnits></UnitValues><Analysis>
            <Enums /><Counts /><New /></Analysis></ValueMetadata>'
          ),
          NA
        ),
        i_form_name = .data$form_name,
        i_section_name = .data$section_header,
        i_field_name = .data$field_name,
        i_field_type = .data$field_type,
        i_identifier = .data$identifier,
        i_data_field_name = .data$data_field_name,
        i_text_validation = .data$text_validation_type_or_show_slider_number,
        i_text_validation_min = .data$text_validation_min,
        i_text_validation_max = .data$text_validation_max,
        i_branching_logic = .data$branching_logic,
        i_field_annotation = .data$field_annotation
      ) |>
      unique()
  )



  #LEVEL 4: FIELD_CHOICES
  concept_staging <- dplyr::bind_rows(
    concept_staging,
    dd |>
      dplyr::filter(!is.na(.data$choice_value)) |>
      tidyr::unite(
        "path",
        "form_name",
        "section_cd",
        "field_name",
        "choice_value",
        sep = "\\",
        na.rm = TRUE,
        remove = FALSE
      ) |>
      tidyr::unite(
        "tooltip",
        "instrument_label",
        "section_header",
        "field_label",
        "choice_label",
        sep = " \\ ",
        na.rm = TRUE,
        remove = FALSE
      ) |>
      dplyr::transmute(
        C_BASECODE = stringr::str_glue("{concept_cd_prefix}{data_field_name}"),
        C_NAME =
          stringr::str_glue("{field_label} ({choice_value}, {choice_label})"),
        C_FULLNAME = paste0("\\", project_id, "\\", .data$path, "\\"),
        C_VISUALATTRIBUTES = "LAE",
        C_HLEVEL = stringr::str_count(.data$C_FULLNAME, "\\\\") - 1,
        C_TOOLTIP = stringr::str_glue("{project_id} \\ {tooltip}"),
        i_form_name = .data$form_name,
        i_section_name = .data$section_header,
        i_field_name = .data$field_name,
        i_field_type = .data$field_type,
        i_identifier = .data$identifier,
        i_data_field_name = .data$data_field_name,
        i_choice_value = .data$choice_value,
        i_choice_label = .data$choice_label
      ) |>
      unique()
  )

  concept_staging

  ## TODO: add the rest of the ontology columns

}
