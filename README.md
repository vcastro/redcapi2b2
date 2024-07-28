
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redcapi2b2

![](redcap_i2b2_heart.jpg)

<!-- badges: start -->
<!-- badges: end -->

redcapi2b2 is an R package with functions to translate REDCap data to
the i2b2 common data model (CDM). REDCap is a popular electronic data
capture tool widely used in clinical research. i2b2 is a clinical data
repository tool with a query tool and data export capabilities that
enable users to construct complex queries and export analysis-ready
files.

## Installation

You can install the development version of redcapi2b2 like so:

``` r
remotes::install_github("vcastro/redcapi2b2")
```

## Features of redcapi2b2

- Tidy REDCap data and data dictionaries expanding single-choice fields
  to multiple concepts.  
- Support for both cross-sectional and longitudinal projects and
  repeating forms
- Ability to customize which REDCap fields to use for i2b2 concept
  start_date
- Customization of REDCap calculated fields
- Test data with example usage
- Support for very large REDCap projects with 90+ forms and 15K records
- Include form sections in the i2b2 ontology and ability to customize
  sections.
- Construct i2b2 C_METADAXML based on REDCap text_validation to enable
  value constraint queries.

## Example

A sample redcap dictionary and fictitious data from the RECOVER study
are included with the package. First we tidy the dataset and dictionary
and then we can generate i2b2 ontology and observation_fact tables.

``` r
library(redcapi2b2)

# first tidy the redcap data dictionary
recover_tidy_dictionary <- tidy_redcap_dictionary(
  redcap_dd = recover_adult_enrollment_dd,
  instrument_labels = recover_adult_enrollment_inst
  )

# tidy the redcap data
recover_tidy_data <- tidy_redcap_data(
  redcap_data = recover_adult_enrollment_data,
  redcap_dd = recover_adult_enrollment_dd
  )

# build i2b2 ontology
recover_i2b2_ontology <- redcap_i2b2_ontology(
  redcap_tidy_dd = recover_tidy_dictionary
  )

head(recover_i2b2_ontology[,1:5], 5)
#>               C_BASECODE                                         C_NAME
#> 1                   <NA>                                         REDCap
#> 2          RC:enrollment                                     Enrollment
#> 3           RC:record_id                                (001) Record ID
#> 4 RC:enrollment_fversion                 (002) Enrollment form version:
#> 5 RC:enrollment_fqueries (003) Placeholder to attach form-level queries
#>                                    C_FULLNAME C_VISUALATTRIBUTES C_COMMENT
#> 1                                  \\REDCap\\                 CA        NA
#> 2                      \\REDCap\\enrollment\\                FAE        NA
#> 3           \\REDCap\\enrollment\\record_id\\                LAE        NA
#> 4 \\REDCap\\enrollment\\enrollment_fversion\\                LAE        NA
#> 5 \\REDCap\\enrollment\\enrollment_fqueries\\                LAE        NA
```

``` r

# build i2b2 facts
recover_i2b2_facts <- redcap_i2b2_facts(
  redcap_data = recover_tidy_data,
  redcap_i2b2_ontology = recover_i2b2_ontology,
  date_mappings = list(enrollment= c("enroll_dt", "index_dt"))
)

head(recover_i2b2_facts[,1:5], 5)
#> # A tibble: 5 × 5
#>   PATIENT_IDE   PATIENT_IDE_SOURCE ENCOUNTER_IDE ENCOUNTER_IDE_SOURCE CONCEPT_CD
#>   <chr>         <chr>              <chr>         <chr>                <glue>    
#> 1 RA1S001-00001 REDCap             REDCap|enrol… REDCap               RC:enroll…
#> 2 RA1S001-00001 REDCap             REDCap|enrol… REDCap               RC:elig_yn
#> 3 RA1S001-00001 REDCap             REDCap|enrol… REDCap               RC:enroll…
#> 4 RA1S001-00001 REDCap             REDCap|enrol… REDCap               RC:index_…
#> 5 RA1S001-00001 REDCap             REDCap|enrol… REDCap               RC:enrl_i…
```
