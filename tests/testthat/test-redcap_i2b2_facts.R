test_that("recover adult enrollment - no params", {


  expect_no_error(
    redcap_i2b2_facts(
      redcap_data =
        tidy_redcap_data(
          redcap_data = recover_adult_enrollment_data,
          redcap_dd = recover_adult_enrollment_dd),
      redcap_i2b2_ontology =
        redcap_i2b2_ontology(
          tidy_redcap_dictionary(redcap_dd = recover_adult_enrollment_dd,
                                 instrument_labels = recover_adult_enrollment_inst)
        ),
      date_mappings = list(enrollment= c("enroll_dt", "index_dt"))
    )
  )

})
