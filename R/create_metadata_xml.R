#' Create xml for i2b2 ontology in C_METADATAXML
#'
#' @param version Metadataxml version (default: 3.02)
#' @param creation_datetime Creation datetime in format 01/26/2011 00:00:00
#' @param test_id Code for the value
#' @param test_name Came of the value
#' @param data_type Data type of value.  Allowed data_types are: PosInteger,
#' Integer, PosFloat, Float, Enum, or String
#'@param flags_to_use Allowed valueflag_cds.  Allowed flags N, L, H, A, T.
#'Multiple flags can be specified in a single string (ex: NLH). Set to blank
#'string for no flag constraints.
#' @param ok_to_use_values default to Y.  Any other value will disable value
#' constraints
#' @param unit_values A list of allowable unit values
#' @param enum_values A list of allowable enum values with description
#' @param max_string_length Max character length for string searches.
#'
#' @return XML object
#' @export
#'
#' @examples
#' unit_values <- list(
#'  NormalUnits = "mg/dose",
#'  EqualUnits = c("mg/tablet", "gm/liter"),
#'  ConvertingUnits = list(
#'   list(Units = "gm/tablet", MultiplyingFactor = 1000),
#'   list(Units = "mg/0.5ml", MultiplyingFactor = 2)),
#'  ExcludingUnits = c("%", "iu"))
#'
#'  enum_values <- list(Val = list(
#'   list(description = "Before meals", value = "AC"),
#'   list(description = "Twice per day", value = "BID")),
#'  ExcludingVal = list(list(
#'   description = "Invalid result", value = "pending")))
#'
#'  xml_output <- create_metadata_xml(
#'   version = "3.02",
#'   creation_datetime = "01/26/2011 00:00:00",
#'   test_id = "MED:DOSE",
#'   test_name = "Medication Dose",
#'   data_type = "PosFloat",
#'   flags_to_use = "NHL",
#'   ok_to_use_values = "Y",
#'   unit_values = unit_values,
#'   enum_values = enum_values
#'   )
#'
#'   cat(xml_output)
#' @source https://community.i2b2.org/wiki/display/DevForum/Metadata+XML+for+Medication+Modifiers
create_metadata_xml <- function(version = "3.02",
                                creation_datetime,
                                test_id,
                                test_name = test_id,
                                data_type,
                                flags_to_use,
                                ok_to_use_values = "Y",
                                unit_values = list(),
                                enum_values = list(),
                                max_string_length = NULL) {

  # Create the root node
  root <- XML::newXMLNode("ValueMetadata")

  # Add child nodes
  XML::newXMLNode("Version", version, parent = root)
  XML::newXMLNode("CreationDateTime", creation_datetime, parent = root)
  XML::newXMLNode("TestID", test_id, parent = root)
  XML::newXMLNode("TestName", test_name, parent = root)
  XML::newXMLNode("DataType", data_type, parent = root)
  XML::newXMLNode("Flagstouse", flags_to_use, parent = root)
  XML::newXMLNode("Oktousevalues", ok_to_use_values, parent = root)

  # UnitValues node
  unit_values_node <- XML::newXMLNode("UnitValues", parent = root)
  if (!is.null(unit_values$NormalUnits)) {
    XML::newXMLNode("NormalUnits",
                    unit_values$NormalUnits,
                    parent = unit_values_node)
  }
  if (!is.null(unit_values$EqualUnits)) {
    for (equal_unit in unit_values$EqualUnits) {
      XML::newXMLNode("EqualUnits", equal_unit, parent = unit_values_node)
    }
  }
  if (!is.null(unit_values$ConvertingUnits)) {
    for (converting_unit in unit_values$ConvertingUnits) {
      converting_unit_node <- XML::newXMLNode("ConvertingUnits",
                                              parent = unit_values_node)
      XML::newXMLNode("Units",
                      converting_unit$Units,
                      parent = converting_unit_node)
      XML::newXMLNode("MultiplyingFactor",
                      converting_unit$MultiplyingFactor,
                      parent = converting_unit_node)
    }
  }
  if (!is.null(unit_values$ExcludingUnits)) {
    for (excluding_unit in unit_values$ExcludingUnits) {
      XML::newXMLNode("ExcludingUnits",
                      excluding_unit,
                      parent = unit_values_node)
    }
  }

  # EnumValues node
  enum_values_node <- XML::newXMLNode("EnumValues", parent = root)
  if (!is.null(enum_values$Val)) {
    for (val in enum_values$Val) {
      XML::newXMLNode(
        "Val",
        attrs = c(description = val$description),
        val$value,
        parent = enum_values_node
      )
    }
  }
  if (!is.null(enum_values$ExcludingVal)) {
    for (excluding_val in enum_values$ExcludingVal) {
      XML::newXMLNode(
        "ExcludingVal",
        attrs = c(description = excluding_val$description),
        excluding_val$value,
        parent = enum_values_node
      )
    }
  }

  # Optional MaxStringLength node
  if (!is.null(max_string_length)) {
    XML::newXMLNode("MaxStringLength", max_string_length, parent = root)
  }

  # Convert the XML tree to a string
  xml_string <- XML::saveXML(root, indent = TRUE)
  return(xml_string)
}
