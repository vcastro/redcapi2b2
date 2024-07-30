strip_html <- function(html_string) {
  gsub("<[^>]*>", "", html_string)
}


rename_dd_column <- function(df) {

  col_mappings <- list(
    "Variable / Field Name" = "field_name",
    "Form Name" = "form_name",
    "Section Header" = "section_header",
    "Field Type" = "field_type",
    "Field Label" = "field_label",
    "Choices, Calculations, OR Slider Labels" = "select_choices_or_calculations",
    "Field Note" = "field_note",
    "Text Validation Type OR Show Slider Number" = "text_validation_type_or_show_slider_number",
    "Text Validation Min" = "text_validation_min",
    "Text Validation Max" = "text_validation_max",
    "Identifier?" = "identifier",
    "Branching Logic (Show field only if...)" = "branching_logic",
    "Required Field?" = "required_field",
    "Custom Alignment" = "custom_alignment",
    "Question Number (surveys only)" = "question_number",
    "Matrix Group Name" = "matrix_group_name",
    "Matrix Ranking?" = "matrix_ranking",
    "Field Annotation" = "field_annotation"
  )

  for (original_name in names(col_mappings)) {
    new_name <- col_mappings[[original_name]]
    # Check if the original name exists in the data frame
    if (original_name %in% colnames(df)) {
      colnames(df)[colnames(df) == original_name] <- new_name
    }
  }

  return(df)
}
