#' Tidy REDCap data
#'
#' @param redcap_data a REDCap data file
#' @param redcap_dd a REDCap data dictionary
#'
#' @return A data.frame
#' @export
#'
#' @importFrom rlang .data
tidy_redcap_data <- function(redcap_data, redcap_dd) {

  radio_fields <- redcap_dd |>
    dplyr::filter(.data$field_type %in% c("radio", "dropdown", "yesno")) |>
    dplyr::pull(.data$field_name)

  if (length(radio_fields) > 0) {
    d_expanded <- add_radio_dummy_var(redcap_data, radio_fields)
  } else {
    d_expanded <- redcap_data
  }

  d_expanded
}



#' @importFrom rlang .data
add_radio_dummy_var <- function(d, radio_fields) {

  d_id <- d |>
    tibble::rowid_to_column("row_id")

  d_radiodummy <- d_id |>
    dplyr::select("row_id", tidyselect::any_of(radio_fields)) |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(radio_fields),
                                as.character)) |>
    tidyr::pivot_longer(
      tidyselect::any_of(radio_fields),
      names_to = "field_name",
      values_to = "value",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(value = sub("-", "_", .data$value), indicator = 1) |>
    tidyr::pivot_wider(
      names_from = c("field_name", "value"),
      names_sep = "___",
      values_from = "indicator",
      values_fill = 0
    )

  d_id |>
    dplyr::select(-tidyselect::any_of(radio_fields)) |>
    dplyr::left_join(d_radiodummy, by = "row_id")
}
