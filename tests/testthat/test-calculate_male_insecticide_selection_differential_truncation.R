test_that("equation works", {
  expect_equal(calculate_male_insecticide_selection_differential_truncation(male.trait.mean.after.selection = 12,
                                                                                       male.trait.mean = 10), 2)
})
