test_that("all die no selection differential", {
  expect_equal(calculate_female_exposed_selection_differential_truncation(field.survival = 0,
                                                                          standard.deviation = 20), 0)
})


test_that("all survive no selection differential", {
  expect_equal(calculate_female_exposed_selection_differential_truncation(field.survival = 1,
                                                                          standard.deviation = 20), 0)
})
