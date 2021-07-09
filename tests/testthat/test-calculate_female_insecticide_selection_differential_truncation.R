test_that("equation works", {
  expect_equal(calculate_female_insecticide_selection_differential_truncation(female.trait.mean.after.selection = 10,
                                                                              female.trait.mean = 1), 9)

  expect_equal(calculate_female_insecticide_selection_differential_truncation(female.trait.mean.after.selection = 1,
                                                                              female.trait.mean = 10), -9)

  expect_equal(calculate_female_insecticide_selection_differential_truncation(female.trait.mean.after.selection = 1,
                                                                              female.trait.mean = 1), 0)

})
