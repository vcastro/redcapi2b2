test_that("recover adult enrollment - no params", {

  expect_no_error(
    redcap_i2b2_facts(
      redcap_data =
        tidy_redcap_data(redcap_data = recover_adult_enrollment_data,
                         redcap_dd = recover_adult_enrollment_dd),
      redcap_i2b2_ontology =
        redcap_i2b2_ontology(
          tidy_redcap_dictionary(
            redcap_dd = recover_adult_enrollment_dd,
            instrument_labels = recover_adult_enrollment_inst)
        ),
      date_mappings = list(enrollment = c("enroll_dt", "index_dt"))
    )
  )

})

test_that("recover adult enrollment - invalid ontology throws error", {

  expect_error(
    redcap_i2b2_facts(
      redcap_data =
        tidy_redcap_data(redcap_data = recover_adult_enrollment_data,
                         redcap_dd = recover_adult_enrollment_dd),
      redcap_i2b2_ontology = recover_adult_enrollment_dd,
      date_mappings = list(enrollment = c("enroll_dt", "index_dt"))
    )
  )

})


test_that("recover adult enrollment - invalid date_mappings throws error", {

  expect_error(
    redcap_i2b2_facts(
      redcap_data =
        tidy_redcap_data(redcap_data = recover_adult_enrollment_data,
                         redcap_dd = recover_adult_enrollment_dd),
      redcap_i2b2_ontology =
        redcap_i2b2_ontology(
          tidy_redcap_dictionary(
            redcap_dd = recover_adult_enrollment_dd,
            instrument_labels = recover_adult_enrollment_inst)
        ),
      date_mappings = list(enrollment = c("bad_dt_field"))
    )
  )


})


test_that("recover adult enrollment - one bad date does not throw error", {

  expect_no_error(
    redcap_i2b2_facts(
      redcap_data =
        tidy_redcap_data(redcap_data = recover_adult_enrollment_data,
                         redcap_dd = recover_adult_enrollment_dd),
      redcap_i2b2_ontology =
        redcap_i2b2_ontology(
          tidy_redcap_dictionary(
            redcap_dd = recover_adult_enrollment_dd,
            instrument_labels = recover_adult_enrollment_inst)
        ),
      date_mappings = list(enrollment = c("enroll_dt", "bad_dt_field"))
    )
  )
})


#TODO: RECOVER adult more, RECOVER autopsy, eMERGE
