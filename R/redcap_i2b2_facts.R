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
                              date_mappings = NA,
                              debug = FALSE,
                              batch_size = 200) {

  redcap_default_fields <- c(
    "record_id",
  #  "redcap_event_name",
    "redcap_repeat_instance",
    "redcap_repeat_instrument"
   # "redcap_data_access_group"
  )


  if (!inherits(redcap_i2b2_ontology, "redcap_i2b2_ontology")) {
    stop("You must pass a valid i2b2 ontology.'")
  }

  #TODO: warning for data not in dd
  #TODO: check for valid dates
  #TODO: test dates to multiple forms

  # iterate through columns to get R class data_types from the data.frame
  data_types <-
    purrr::map_df(redcap_data, class)[1, ] |>
    tidyr::pivot_longer(tidyselect::everything(),
                        names_to = "data_field_name",
                        values_to = "df_data_type")


  form_date <- function(redcap_data,
                        form_name,
                        date_fields,
                        redcap_default_fields) {


    date_cols <- rlang::syms(intersect(date_fields, names(redcap_data)))

    if (length(date_cols) == 0) {
      stop(stringr::str_glue("Data fields for {form_name} do not exist in the
                             data."))
    }

    redcap_data |>
      dplyr::mutate(
        form_name = form_name,
        start_date = dplyr::coalesce(!!!date_cols),
        start_date = lubridate::mdy_hms(.data$start_date, truncated = 3)
      ) |>
      dplyr::filter(!is.na(.data$start_date)) |>
      dplyr::select(tidyselect::any_of(c(
        redcap_default_fields, "form_name", "start_date"
      )))

  }

  if (!is.na(date_mappings)) {
    ##iterate through named list
    date_fields <-
      purrr::imap(date_mappings,
                  ~ form_date(redcap_data, .y, .x, redcap_default_fields)) |>
      dplyr::bind_rows()
  } else {
    date_fields <- redcap_i2b2_ontology |>
      group_by(i_form_name) |>
      summarize(start_date = Sys.Date(), .groups = "drop")
  }

  # pivot wide file to tall
  redcap_data_long <- redcap_data |>
    tidyr::pivot_longer(
      cols = -tidyselect::any_of(redcap_default_fields),
      names_to = "data_field_name",
      values_to = "value",
      values_transform = as.character,
      values_drop_na = TRUE
    ) |>
    dplyr::filter(.data$data_field_name != "row_id") |>
    dplyr::filter(!(stringr::str_detect(.data$data_field_name, "__") &
                      .data$value == "0"))




  field_metadata <- redcap_data_long |>
    dplyr::group_by(.data$data_field_name) |>
    dplyr::summarize(n = dplyr::n(),
                     max_field_len = max(stringr::str_length(.data$value), na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::full_join(data_types, by = "data_field_name") |>
    dplyr::inner_join(redcap_i2b2_ontology,
                      by = c("data_field_name" = "i_data_field_name")) |>
    dplyr::left_join(date_fields, by = c("i_form_name" = "i_form_name")) |>
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
            stringr::str_length(.data$max_field_len) <= 255 ~ "text",
          .data$i_field_type %in% c("notes") &
            stringr::str_length(.data$max_field_len) > 255 ~ "blob",
          .data$i_field_type == "text" &
            .data$df_data_type %in% c("POSIXct", "Date", "hms") ~ "date",
          TRUE ~ NA
        )
    )

  # Observation Fact Processing in Batches

  # Define batch processing
  # Set the batch size and file path for intermediate storage

  output_file <- tempfile("observation_fact_batches", fileext = ".csv")
  unique_fields <- unique(redcap_data_long$data_field_name)

  # Write headers to the output file initially (optional, for formatted CSV)
  write.table(
    data.frame(
      PATIENT_IDE = character(),
      PATIENT_IDE_SOURCE = character(),
      ENCOUNTER_IDE = character(),
      ENCOUNTER_IDE_SOURCE = character(),
      CONCEPT_CD = character(),
      START_DATE = as.Date(character()),
      END_DATE = as.Date(character()),
      PROVIDER_ID = character(),
      MODIFIER_CD = character(),
      INSTANCE_NUM = numeric(),
      VALTYPE_CD = character(),
      NVAL_NUM = numeric(),
      TVAL_CHAR = character(),
      VALUEFLAG_CD = character(),
      QUANTITY_NUM = numeric(),
      UNITS_CD = character(),
      LOCATION_CD = character(),
      CONFIDENCE_NUM = numeric(),
      OBSERVATION_BLOB = character(),
      UPDATE_DATE = as.Date(character()),
      DOWNLOAD_DATE = as.Date(character()),
      IMPORT_DATE = as.Date(character()),
      SOURCESYSTEM_CD = character(),
      UPLOAD_ID = integer()
    ),
    file = output_file,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    append = FALSE
  )

  batch_num <- 1L
  # Process and write each batch to disk
  for (fields in split(unique_fields, ceiling(seq_along(unique_fields) / batch_size))) {

    batch_result <- redcap_data_long %>%
      dplyr::filter(data_field_name %in% fields) %>%
      dplyr::inner_join(field_metadata, by = "data_field_name") %>%
      dplyr::transmute(
        PATIENT_IDE = record_id,
        PATIENT_IDE_SOURCE = project_id,
        ENCOUNTER_IDE = ifelse(
          "redcap_event_name" %in% names(redcap_data),
          str_glue("{project_id}|{record_id}|{redcap_event_name}"),
          str_glue("{project_id}|{record_id}")
        ),
        ENCOUNTER_IDE_SOURCE = project_id,
        CONCEPT_CD = C_BASECODE,
        START_DATE = start_date,
        END_DATE = NA,
        PROVIDER_ID = ifelse(
          "redcap_data_access_group" %in% names(redcap_data),
          redcap_data_access_group,
          "@"
        ),
        MODIFIER_CD = "@",
        INSTANCE_NUM = ifelse(
          "redcap_repeat_instance" %in% names(redcap_data),
          coalesce(redcap_repeat_instance, 1),
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
          .data$i2b2_field_type == "numeric" &
            .data$df_data_type %in% c("numeric", "integer"),
          .data$value,
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
        OBSERVATION_BLOB = ifelse(i2b2_field_type == "blob", value, NA_character_),
        UPDATE_DATE = Sys.Date(),
        DOWNLOAD_DATE = Sys.Date(),
        IMPORT_DATE = Sys.Date(),
        SOURCESYSTEM_CD = stringr::str_glue("{project_id}|redcapi2b2"),
        UPLOAD_ID = NA_integer_
      )

    if(debug) {
      message(stringr::str_glue("Writing batch {batch_num} to {output_file}"))
    }

    # Append batch result to the output file
    write.table(batch_result, file = output_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)

    # Clear batch result from memory
    rm(batch_result)
    gc()  # Clean up memory

    batch_num <- batch_num+1
  }

  read.csv(output_file)
}
