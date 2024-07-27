test_that("recover adult enrollment - no params", {

  expect_no_error(
    tidy_redcap_data(redcap_data = recover_adult_enrollment_data,
                     redcap_dd = recover_adult_enrollment_dd)
  )

})
