test_that("recover adult enrollment - no params", {

  expect_no_error(
    tidy_redcap_dictionary(redcap_dd = recover_adult_enrollment_dd,
                           instrument_labels = recover_adult_enrollment_inst)
  )

})
