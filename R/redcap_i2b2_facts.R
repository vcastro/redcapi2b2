#' Build an i2b2 observation_fact table from a REDCap tidy data and dictionary
#'
#' @param redcap_data REDCap tidy data
#' @param redcap_i2b2_ontology i2b2 ontology (usually from output of
#' redcap_i2b2_ontology())
#' @param project_id Project name to use as the root level (default: REDCap)
#' @param date_mappings Named list with fields to use for the start_date of
#' each form
#'
#' @return data.frame
#' @export
#' @importFrom rlang .data
redcap_i2b2_facts <- function(redcap_data,
                              redcap_i2b2_ontology,
                              project_id = "REDCap",
                              date_mappings) {

  redcap_default_fields <- c(
    "record_id",
    "redcap_event_name",
    "redcap_repeat_instance",
    "redcap_repeat_instrument",
    "redcap_data_access_group"
  )

  #TODO: warning for data not in dd
  #TODO: check for valid dates

  # iterate through columns to get R class data_types from the data.frame
  data_types <-
    purrr::map_df(redcap_data, class)[1, ] |>
    tidyr::pivot_longer(tidyselect::everything(),
                        names_to = "data_field_name",
                        values_to = "df_data_type")

  #TODO: implement date fields
  date_fields <- redcap_data |>
    dplyr::select(tidyselect::any_of(c(
      redcap_default_fields, unlist(date_mappings, use.names = FALSE)
    ))) |>
    dplyr::mutate(
      start_date = dplyr::coalesce(!!! rlang::syms(date_mappings$enrollment))
    )


  form_date <- function(redcap_data,
                        form_name,
                        date_fields,
                        redcap_default_fields) {
    redcap_data |>
      dplyr::mutate(
        form_name = form_name,
        start_date = dplyr::coalesce(!!!rlang::syms(date_fields)),
        start_date = lubridate::mdy_hms(.data$start_date, truncated = 3)
      ) |>
      dplyr::filter(!is.na(.data$start_date)) |>
      dplyr::select(tidyselect::any_of(c(
        redcap_default_fields, "form_name", "start_date"
      )))

  }


  ##iterate through named list
  date_fields <-
    purrr::imap(date_mappings,
                ~ form_date(redcap_data, .y, .x, redcap_default_fields)) |>
    dplyr::bind_rows()

  # pivot wide file to tall
  redcap_data_long <- redcap_data |>
    tidyr::pivot_longer(
      cols = -tidyselect::any_of(redcap_default_fields),
      names_to = "data_field_name",
      values_to = "value",
      values_transform = as.character,
      values_drop_na = TRUE
    ) |>
    dplyr::filter(.data$data_field_name != "row_id")



  field_metadata <- redcap_data_long |>
    dplyr::group_by(.data$data_field_name) |>
    dplyr::summarize(n = dplyr::n(),
                     max_field_len = max(stringr::str_length(.data$value),
                                         na.rm = TRUE)) |>
    dplyr::full_join(data_types, by = "data_field_name")


  #TODO: add *_complete fields
  redcap_data_long_mapped <- redcap_data_long |>
    dplyr::inner_join(field_metadata, by = "data_field_name") |>
    dplyr::inner_join(redcap_i2b2_ontology,
                      by = c("data_field_name" = "i_data_field_name")) |>
    dplyr::left_join(date_fields,
                     by = c(redcap_default_fields,
                            "i_form_name" = "form_name")) |>
    dplyr::mutate(
      i2b2_field_type =
        dplyr::case_when(
          .data$i_field_type %in%
            c("radio", "checkbox", "dropdown", "yesno") ~ "choice",
          .data$i_field_type %in% c("text", "calc") &
            (
              .data$i_text_validation == "integer" |
                stringr::str_starts(.data$i_text_validation, "number") |
                .data$df_data_type %in% c("numeric", "integer")
            ) ~ "numeric",
          .data$i_field_type %in% c("text", "notes") &
            .data$df_data_type == "character" &
            stringr::str_length(.data$value) <= 255 ~ "text",
          .data$i_field_type %in% c("notes") &
            stringr::str_length(.data$value) > 255 ~ "blob",
          .data$i_field_type == "text" &
            .data$df_data_type %in% c("POSIXct", "Date", "hms") ~ "date",
          TRUE ~ NA
        )
    )


  observation_fact <- redcap_data_long_mapped |>
    dplyr::transmute(
      PATIENT_IDE = .data$record_id,
      PATIENT_IDE_SOURCE = project_id,
      ENCOUNTER_IDE = ifelse(
        "redcap_event_name" %in% names(redcap_data),
        stringr::str_glue("{project_id}|{redcap_event_name}"),
        project_id
      ),
      ENCOUNTER_IDE_SOURCE = project_id,
      CONCEPT_CD = .data$C_BASECODE,
      START_DATE = .data$start_date,
      END_DATE = NA,
      PROVIDER_ID = ifelse(
        "redcap_data_access_group" %in% names(redcap_data),
        .data$redcap_data_access_group,
        "@"
      ),
      MODIFIER_CD = "@",
      INSTANCE_NUM = ifelse(
        "redcap_repeat_instance" %in% names(redcap_data),
        dplyr::coalesce(.data$redcap_repeat_instance, 1),
        1
      ),
      VALTYPE_CD = dplyr::case_when(
        .data$i2b2_field_type == "numeric" ~ "N",
        .data$i2b2_field_type == "text" ~ "T",
        .data$i2b2_field_type == "blob" ~ "B",
        .data$i2b2_field_type == "data" ~ "T",
        TRUE ~ NA_character_
      ),
      NVAL_NUM = ifelse(
        .data$i2b2_field_type == "numeric",
        ifelse(is.numeric(.data$value), as.numeric(.data$value), NA_real_),
        NA_real_
      ),
      TVAL_CHAR = dplyr::case_when(
        .data$i2b2_field_type == "text" ~ .data$value,
        .data$i2b2_field_type == "numeric" ~ "E",
        .data$i2b2_field_type == "date" ~ .data$value,
        TRUE ~ NA_character_
      ),
      VALUEFLAG_CD = NA_character_,
      QUANTITY_NUM = NA_real_,
      UNITS_CD = NA_character_,
      LOCATION_CD = NA_character_,
      CONFIDENCE_NUM = NA_real_,
      OBSERVATION_BLOB = ifelse(.data$i2b2_field_type == "blob",
                                .data$value,
                                NA_character_),
      UPDATE_DATE = Sys.Date(),
      DOWNLOAD_DATE = Sys.Date(),
      IMPORT_DATE = Sys.Date(),
      SOURCESYSTEM_CD = stringr::str_glue("{project_id}|redcapi2b2"),
      UPLOAD_ID = NA_integer_
    )

  observation_fact

}
