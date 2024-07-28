#' Tidy REDCap Data Dictionary
#'
#' @param redcap_dd REDCap data dictionary
#' @param instrument_labels REDCap instrument labels
#' @param redcap_vars REDCap vars file
#' @param section_mappings Custom section mappings
#' @param calc_field_settings Custom calculated field settings
#'
#' @return data.frame
#' @export
#' @importFrom rlang .data
tidy_redcap_dictionary <- function(redcap_dd,
                                   instrument_labels = NULL,
                                   redcap_vars = NA,
                                   section_mappings = NULL,
                                   calc_field_settings = NULL) {

  # preserve order of fields
  dd <- redcap_dd |>
    tibble::rowid_to_column(var = "file_order")


  # yesno fields need explicit definition of the choices
  dd <- dd |>
    dplyr::mutate(
      select_choices_or_calculations = ifelse(
        .data$field_type == "yesno",
        "0, No | 1, Yes",
        .data$select_choices_or_calculations
      )
    )


  # allow custom definitions of calc fields.  calc fields are now added as text
  # by default. customization: define choices, define text_validation (min,max)
  if (!is.null(calc_field_settings)) {
    # change field type to radio; replace choices
    dd <- dd |>
      dplyr::left_join(calc_field_settings, by = "field_name") |>
      dplyr::mutate(
        field_type = ifelse(
          .data$field_type == "calc" & !is.na(.data$calc_field_type),
          .data$calc_field_type,
          .data$field_type
        ),
        select_choices_or_calculations = ifelse(
          !is.na(.data$calc_field_choices),
          .data$calc_field_choices,
          .data$select_choices_or_calculations
        )
      )

  }

  # expand choice fields into distinct metadata rows
  if (!is.na(redcap_vars)) {
    #TODO: use vars file for more reliable mapping
  } else {
    m_expanded <- expand_metadata_choices(dd)
  }

  # add nice form name from the instrument_labels
  if (!is.null(instrument_labels)) {
    m_expanded <- m_expanded |>
      dplyr::left_join(instrument_labels,
                       by = c("form_name" = "instrument_name"))
  } else {
    m_expanded <- m_expanded |>
      dplyr::mutate(instrument_label = .data$form_name)
  }

  # add custom section mappings
  if (!is.null(section_mappings)) {

    # Translate field_name is mapping file to position
    section_positions <- section_mappings |>
      tidyr::pivot_longer(
        c("section_field_start", "section_field_end"),
        values_to = "field_name",
        names_to = "attr"
      ) |>
      dplyr::inner_join(m_expanded, by = "field_name") |>
      dplyr::select("section_name",
                    "section_cd",
                    "attr",
                    "file_order") |>
      unique() |>
      tidyr::pivot_wider(
        id_cols = c("section_name", "section_cd"),
        names_from = "attr",
        values_from = "file_order"
      )

    # Assign section header by file_order
    section_names <- section_positions |>
      dplyr::mutate(dummy = TRUE) |>
      dplyr::left_join(m_expanded |>
                         dplyr::mutate(dummy = TRUE), by = "dummy") |>
      dplyr::filter(
        .data$file_order >= .data$section_field_start &
          .data$file_order <= .data$section_field_end
      ) |>
      dplyr::select("data_field_name", "section_name", "section_cd")

    m_expanded <- m_expanded |>
      dplyr::left_join(section_names, by = c("data_field_name")) |>
      dplyr::mutate(
        section_header = ifelse(
          !is.na(.data$section_name),
          .data$section_name,
          .data$section_header
        ),
        section_cd = ifelse(
          !is.na(.data$section_cd),
          .data$section_cd,
          stringr::str_sub(.data$section_name, 1, 50)
        )
      ) |>
      dplyr::select("section_name")


  } else {

    m_expanded <- m_expanded |>
      dplyr::mutate(section_cd = NA)
  }


  m_expanded <- m_expanded |>
    dplyr::group_by(.data$form_name) |>
    dplyr::arrange(.data$file_order) |>
    tidyr::fill("section_header") |>
    dplyr::ungroup() |>
    dplyr::mutate(section_cd = ifelse(
      is.na(.data$section_cd) &
        !is.na(.data$section_header),
      .data$section_header,
      .data$section_cd
    ))

  form_order <- m_expanded |>
    dplyr::group_by(.data$form_name) |>
    dplyr::summarize(start_line = min(.data$file_order), .groups = "drop") |>
    dplyr::mutate(form_order = dplyr::min_rank(.data$start_line)) |>
    dplyr::select(-"start_line")

  section_order <- m_expanded |>
    dplyr::group_by(.data$form_name, .data$section_header) |>
    dplyr::summarize(start_line = min(.data$file_order),
                     .groups = "drop_last") |>
    dplyr::mutate(section_order = dplyr::dense_rank(.data$start_line)) |>
    dplyr::select(-"start_line")

  field_order <- m_expanded |>
    dplyr::group_by(.data$form_name, .data$section_header, .data$field_name) |>
    dplyr::summarize(start_line = min(.data$file_order),
                     .groups = "drop_last") |>
    dplyr::mutate(field_order = dplyr::min_rank(.data$start_line)) |>
    dplyr::select(-"start_line")

  choice_order <- m_expanded |>
    dplyr::filter(!is.na(.data$choice_value)) |>
    dplyr::group_by(.data$field_name, .data$choice_value) |>
    dplyr::summarize(start_line = min(.data$file_order),
                     .groups = "drop_last") |>
    dplyr::mutate(choice_order = dplyr::min_rank(.data$choice_value)) |>
    dplyr::select(-"start_line")


  tidy_dd <- m_expanded |>
    dplyr::left_join(form_order, by = "form_name") |>
    dplyr::left_join(section_order, by = c("form_name", "section_header")) |>
    dplyr::left_join(field_order,
                     by = c("form_name", "section_header", "field_name")) |>
    dplyr::left_join(choice_order, by = c("field_name", "choice_value"))

  tidy_dd
}




### parse choice fields to separate rows
### data_field_name is {field_name}___{choice_value} (replacing - with _)
expand_metadata_choices <- function(metadata) {

  m_expanded <- metadata |>
    dplyr::filter(.data$field_type %in%
                    c("radio", "checkbox", "dropdown", "yesno")) |>
    tidyr::separate_rows("select_choices_or_calculations", sep = "\\|") |>
    dplyr::mutate(
      choice_value = stringr::str_trim(sub(
        ",.*", "", .data$select_choices_or_calculations
      )),
      choice_label = stringr::str_trim(sub(
        ".*?,", "", .data$select_choices_or_calculations
      )),
      data_field_name = stringr::str_glue("{field_name}___{choice_value}"),
      data_field_name = sub("-", "_", .data$data_field_name)
    )



  m_expanded |>
    dplyr::bind_rows(metadata |>
                       dplyr::mutate(data_field_name = .data$field_name))

}
