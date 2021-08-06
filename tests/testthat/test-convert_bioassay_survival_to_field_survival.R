test_that("no insecticide all survive works", {
  expect_equal(convert_bioassay_survival_to_field_survival(bioassay.survival = 0,
                                                           regression.coefficient = 1,
                                                           regression.intercept = 1,
                                                           current.insecticide.efficacy = 0), 1)
})

test_that("regression coefficient works", {
  expect_equal(convert_bioassay_survival_to_field_survival(bioassay.survival = 1,
                                                           regression.coefficient = 1,
                                                           regression.intercept = 0,
                                                           current.insecticide.efficacy = 1), 1)
})

test_that("regression intercept works", {
  expect_equal(convert_bioassay_survival_to_field_survival(bioassay.survival = 1,
                                                           regression.coefficient = 0,
                                                           regression.intercept = 1,
                                                           current.insecticide.efficacy = 1), 1)
})

test_that("all die", {
  expect_equal(convert_bioassay_survival_to_field_survival(bioassay.survival = 0,
                                                           regression.coefficient = 1,
                                                           regression.intercept = 0,
                                                           current.insecticide.efficacy = 1), 0)
})
