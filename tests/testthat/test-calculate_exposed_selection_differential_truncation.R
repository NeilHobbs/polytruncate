test_that("all surviving no selection differential", {
  expect_equal(calculate_exposed_selection_differential_truncation(standard.deviation = 20,
                                                                   field.survival = 1), 0)
})


test_that("all dying no selection differential", {
  expect_equal(calculate_exposed_selection_differential_truncation(standard.deviation = 20,
                                                                   field.survival = 0), 0)
})



