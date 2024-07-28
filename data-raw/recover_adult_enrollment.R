## code to prepare RECOVER Adult Enrollment test dataset
## these files were pulled from the RECOVER REDCap test environment on 20240708

recover_adult_enrollment_data <-
  read.csv("data-raw/recover_adult_enrollment_data.csv")

recover_adult_enrollment_dd <-
  read.csv("data-raw/recover_adult_enrollment_dd.csv")

recover_adult_enrollment_inst <-
  read.csv("data-raw/recover_adult_enrollment_inst.csv")

usethis::use_data(recover_adult_enrollment_data, overwrite = TRUE)
usethis::use_data(recover_adult_enrollment_dd, overwrite = TRUE)
usethis::use_data(recover_adult_enrollment_inst, overwrite = TRUE)
