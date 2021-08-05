test_that("no selection", {
  expect_equal(calculate_male_exposed_selection_differential_truncation(field.survival = 1,
                                                                        standard.deviation = 1), 0)
})

test_that("no survival no selection differential", {
  expect_equal(calculate_male_exposed_selection_differential_truncation(field.survival = 0,
                                                                        standard.deviation = 1), 0)
})
