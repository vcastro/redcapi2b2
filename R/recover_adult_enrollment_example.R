#' RECOVER Adult Enrollment Data
#'
#' Test data from the RECOVER Adult Observational Study.  The data subset
#' only includes REDCap fields from the study enrollment REDCap instrument.
#' This is fictional data and does not include any real participant information.
#' The data was pulled via REDCap API from the RECOVER REDCap Sandbox on
#' 2024-07-08.
#'
#' @format ## `recover_adult_enrollment_data`
#' A data frame with 211 rows and 39 columns:
#' \describe{
#'   \item{record_id}{RECOVER participant ID}
#'   \item{redcap_event_name}{The REDCap event name.  Enrollment data will only
#'   exist in the enrollment_arm_1 event but the dataset includes other events
#'   for testing and development purposes.}
#'   \item{redcap_repeat_instrument}{Form name for repeating instances}
#'   \item{redcap_repeat_instance}{Number of repeat instance}
#'   \item{redcap_data_access_group}{DAG of data entry}
#'   \item{enrollment_fversion - enrollment_complete}{Data from the enrollment
#'   form.  See data dictionary for field definitions}
#'
#' }
"recover_adult_enrollment_data"


#' RECOVER Adult Enrollment Data Dictionary
#'
#' Data dictionary from the RECOVER Adult Longitudinal Study.  The dictionary
#' only include REDCap fields from the study enrollment REDCap instrument.
#' The data was pulled via REDCap API from the RECOVER REDCap Sandbox on
#' 2024-07-08.
#'
#' @format ## `recover_adult_enrollment_dd`
#' A data frame with 44 rows and 18 columns:
#' \describe{
#'   \item{field_name}{Field name}
#'   \item{form_name}{Instrument name}
#'   \item{section_header}{The name of the section}
#'   \item{field_type}{Type of field (e.g. text, radio, checkbox)}
#'   \item{field_label}{Text label of field.  May include HTML and other special
#'   characters supported in REDCap}
#'   \item{select_choices_or_calculations}{Choices for radio and checkbox fields
#'   and calculations for calc field_types}
#'   \item{field_note}{Note for the field}
#'   \item{text_validation_type_or_show_slider_number}{Validation type for text
#'   fields (which can include numeric inputs)}
#'   \item{text_validation_min}{Minimum allowed value in text field}
#'   \item{text_validation_max}{Maximum allowed value in text field}
#'   \item{text_validation_max}{logical indication if the field is tagged as
#'   an identifier}
#'   \item{branching_logic}{Branching logic in REDCap syntax}
#'   \item{required_field}{Is the field required}
#'   \item{custom_alignment}{Position of field on the REDCap form UI}
#'   \item{question_number}{Number of the question}
#'   \item{matrix_group_name}{Grouping for fields organized in a matrix}
#'   \item{matrix_ranking}{Ranking for fields organized in a matrix}
#'   \item{field_annotation}{Annotations and smart field logic}
#'
#' }
"recover_adult_enrollment_dd"


#' RECOVER Adult Enrollment Instrument Labels
#'
#' Instrument labels from the RECOVER Adult Longitudinal Study.  The dictionary
#' only include a few REDCap instrument mappings.
#' The data was pulled via REDCap API from the RECOVER REDCap Sandbox on
#' 2024-07-08.
#'
#' @format ## `recover_adult_enrollment_dd`
#' A data frame with 5 rows and 2 columns:
#' \describe{
#'   \item{instrument_name}{Instrument name corresponding to form_name in data
#'   dictionary}
#'   \item{instrument_label}{Instrument nice label}
#'
#' }
"recover_adult_enrollment_inst"
